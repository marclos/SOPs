#!/usr/bin/env Rscript
# =============================================================================
# merge_sop_pdfs.R
# Merge rendered SOP PDFs into combined documents with hyperlinked TOCs.
#
# Produces:
#   docs/EA-Program-SOPs-Complete.pdf    -- all SOPs
#   docs/EA-Program-Field-SOPs.pdf       -- F-series only
#   docs/EA-Program-Lab-SOPs.pdf         -- L-series only
#   docs/EA-Program-Safety-SOPs.pdf      -- L001--L099 (safety/admin)
#   docs/EA-Program-Molecular-SOPs.pdf   -- L700+ and F700+ (molecular/eDNA)
#
# Usage (after quarto render):
#   Rscript merge_sop_pdfs.R              # uses docs/pdfs/ by default
#   Rscript merge_sop_pdfs.R path/to/pdfs  # custom input directory
#
# Requirements:
#   install.packages(c("qpdf", "rmarkdown", "tinytex"))
#   TinyTeX or a LaTeX distribution with: pdfpages, longtable, xcolor,
#     geometry, hyperref
#
# =============================================================================

library(qpdf)

# =============================================================================
# 0. Find Pandoc
# =============================================================================
quarto_pandoc <- Sys.which("quarto")
if (nzchar(quarto_pandoc)) {
  quarto_bin <- tryCatch(
    system2("quarto", "--paths", stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  pandoc_candidates <- c(
    file.path(dirname(quarto_pandoc), "tools"),
    if (length(quarto_bin) >= 1) file.path(quarto_bin[1], "bin", "tools"),
    dirname(quarto_pandoc)
  )
  for (p in pandoc_candidates) {
    if (file.exists(file.path(p, "pandoc")) ||
        file.exists(file.path(p, "pandoc.exe"))) {
      Sys.setenv(RSTUDIO_PANDOC = p)
      cat(sprintf("Using Pandoc from: %s\n", p))
      break
    }
  }
}

if (!rmarkdown::pandoc_available()) {
  common_paths <- c(
    "/opt/quarto/bin/tools",
    "/usr/local/bin",
    file.path(Sys.getenv("HOME"), ".local", "bin"),
    "/opt/hostedtoolcache/quarto"
  )
  for (p in common_paths) {
    if (file.exists(file.path(p, "pandoc"))) {
      Sys.setenv(RSTUDIO_PANDOC = p)
      cat(sprintf("Using Pandoc from fallback: %s\n", p))
      break
    }
  }
}

if (!rmarkdown::pandoc_available()) {
  pandoc_path <- Sys.which("pandoc")
  if (nzchar(pandoc_path)) {
    Sys.setenv(RSTUDIO_PANDOC = dirname(pandoc_path))
    cat(sprintf("Using Pandoc from system PATH: %s\n", dirname(pandoc_path)))
  } else {
    stop("Pandoc not found. Install Pandoc or Quarto and ensure it is on PATH.")
  }
}

cat(sprintf("Pandoc version: %s\n", rmarkdown::pandoc_version()))

# =============================================================================
# 1. Locate all individual PDFs and read metadata
# =============================================================================
args <- commandArgs(trailingOnly = TRUE)
pdf_dir <- if (length(args) >= 1) args[1] else "docs/pdfs"

if (!dir.exists(pdf_dir)) {
  stop("PDF directory not found: ", pdf_dir,
       "\nRun 'quarto render' first, then try again.")
}

all_pdf_files <- sort(list.files(pdf_dir, pattern = "\\.pdf$",
                                 full.names = TRUE))

if (length(all_pdf_files) == 0) {
  stop("No PDF files found in ", pdf_dir)
}

cat(sprintf("Found %d SOP PDFs in %s\n", length(all_pdf_files), pdf_dir))

# --- Read metadata from .qmd YAML ---
find_qmd <- function(pdf_path) {
  stem <- tools::file_path_sans_ext(basename(pdf_path))
  qmd_candidates <- list.files(
    c("lab_sops", "field_sops", "sops"),
    pattern = paste0("^", stem, "\\.qmd$"),
    full.names = TRUE, recursive = FALSE
  )
  if (length(qmd_candidates) >= 1) qmd_candidates[1] else NA_character_
}

all_page_counts <- vapply(all_pdf_files, function(f) {
  length(qpdf::pdf_length(f))
}, integer(1))

all_titles <- vapply(all_pdf_files, function(f) {
  qmd <- find_qmd(f)
  if (!is.na(qmd)) {
    yaml_front <- rmarkdown::yaml_front_matter(qmd)
    if (!is.null(yaml_front$title)) return(yaml_front$title)
  }
  tools::file_path_sans_ext(basename(f))
}, character(1))

all_sop_numbers <- vapply(all_pdf_files, function(f) {
  qmd <- find_qmd(f)
  if (!is.na(qmd)) {
    yaml_front <- rmarkdown::yaml_front_matter(qmd)
    if (!is.null(yaml_front[["sop-number"]])) return(yaml_front[["sop-number"]])
  }
  tools::file_path_sans_ext(basename(f))
}, character(1))

# Master data frame with all SOPs
all_entries <- data.frame(
  file       = all_pdf_files,
  Number     = all_sop_numbers,
  SOP        = all_titles,
  Pages      = all_page_counts,
  stringsAsFactors = FALSE
)

cat("\nAll SOPs found:\n")
print(all_entries[, c("Number", "SOP", "Pages")], row.names = FALSE)

# =============================================================================
# 2. Helper functions
# =============================================================================
escape_latex <- function(x) {
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("&",  "\\&",  x, fixed = TRUE)
  x <- gsub("%",  "\\%",  x, fixed = TRUE)
  x <- gsub("$",  "\\$",  x, fixed = TRUE)
  x <- gsub("#",  "\\#",  x, fixed = TRUE)
  x <- gsub("_",  "\\_",  x, fixed = TRUE)
  x <- gsub("{",  "\\{",  x, fixed = TRUE)
  x <- gsub("}",  "\\}",  x, fixed = TRUE)
  x <- gsub("~",  "\\textasciitilde{}",  x, fixed = TRUE)
  x <- gsub("^",  "\\textasciicircum{}", x, fixed = TRUE)
  x
}

make_anchor <- function(x) {
  gsub("[^A-Za-z0-9-]", "", x)
}

# ---------------------------------------------------------------------------
# build_combined_pdf()
#   Builds a single combined PDF from a subset of SOPs.
#
#   entries     : data.frame with columns file, Number, SOP, Pages
#   output_file : path for the output PDF
#   toc_title   : title shown on the TOC page
#   toc_subtitle: subtitle shown below the title
# ---------------------------------------------------------------------------
build_combined_pdf <- function(entries, output_file, toc_title, toc_subtitle) {

  if (nrow(entries) == 0) {
    cat(sprintf("  Skipping %s -- no matching SOPs.\n", basename(output_file)))
    return(invisible(NULL))
  }

  cat(sprintf("\n=== Building: %s (%d SOPs) ===\n",
              basename(output_file), nrow(entries)))

  pdf_abs     <- normalizePath(entries$file)
  page_counts <- entries$Pages
  n           <- nrow(entries)
  anchors     <- make_anchor(entries$Number)

  # -- TOC table --
  toc_lines <- sprintf(
    "  \\hyperlink{%s}{%s} & %s \\\\",
    anchors,
    escape_latex(entries$Number),
    escape_latex(entries$SOP)
  )

  toc_table <- paste(
    "\\begin{longtable}{|p{2.2cm}|p{13cm}|}",
    "\\hline",
    "\\textbf{SOP Number} & \\textbf{Title} \\\\",
    "\\hline",
    "\\endhead",
    paste(toc_lines, collapse = "\n  \\hline\n"),
    "\\hline",
    "\\end{longtable}",
    sep = "\n"
  )

  # -- includepdf blocks --
  includepdf_lines <- vapply(seq_len(n), function(i) {
    np <- page_counts[i]
    anchor_cmd <- sprintf("\\hypertarget{%s}{}", anchors[i])
    if (np == 1L) {
      sprintf("\\includepdf[pages=-, pagecommand={%s}]{%s}",
              anchor_cmd, pdf_abs[i])
    } else {
      sprintf(paste0(
        "\\includepdf[pages=1, pagecommand={%s}]{%s}\n",
        "\\includepdf[pages=2-, pagecommand={}]{%s}"),
        anchor_cmd, pdf_abs[i], pdf_abs[i])
    }
  }, character(1))

  # -- Master LaTeX --
  master_tex <- paste0(
'\\documentclass[11pt,letterpaper]{article}

\\usepackage[margin=1in]{geometry}
\\usepackage{pdfpages}
\\usepackage{longtable}
\\usepackage{xcolor}
\\usepackage[
  colorlinks=true,
  linkcolor=eablue,
  urlcolor=eablue,
  bookmarks=true,
  bookmarksopen=true,
  pdfstartview=FitH
]{hyperref}

\\definecolor{eablue}{HTML}{0057B8}
\\pagestyle{empty}

\\begin{document}

\\begin{center}
  {\\Large\\textbf{\\textcolor{eablue}{', escape_latex(toc_title), '}}}\\\\[6pt]
  {\\large ', escape_latex(toc_subtitle), '}\\\\[4pt]
  {\\normalsize ', format(Sys.Date(), "%B %d, %Y"), '}
\\end{center}

\\vspace{12pt}

\\section*{Table of Contents}
\\noindent\\textit{Click any SOP number or title to jump to that procedure.}

\\vspace{6pt}

', toc_table, '

\\newpage

', paste(includepdf_lines, collapse = "\n\n"), '

\\end{document}
')

  # -- Compile --
  build_dir <- tempdir()
  tex_file  <- file.path(build_dir,
                          paste0(tools::file_path_sans_ext(
                            basename(output_file)), ".tex"))
  writeLines(master_tex, tex_file)

  compiled_pdf <- tinytex::latexmk(tex_file, engine = "pdflatex",
                                    clean = TRUE)

  file.copy(compiled_pdf, output_file, overwrite = TRUE)

  total_pages <- length(qpdf::pdf_length(output_file))
  cat(sprintf("  -> %s (%d SOPs, %d pages)\n",
              output_file, nrow(entries), total_pages))

  invisible(output_file)
}

# =============================================================================
# 3. Define the PDF sets
# =============================================================================
# Each set is a list with:
#   filter   : function(entries) -> logical vector selecting rows
#   output   : output filename
#   title    : TOC page title
#   subtitle : TOC page subtitle

output_dir <- dirname(pdf_dir)  # typically "docs"

sets <- list(

  # Complete collection (all SOPs)
  list(
    filter   = function(e) rep(TRUE, nrow(e)),
    output   = file.path(output_dir, "EA-Program-SOPs-Complete.pdf"),
    title    = "EA Program -- Standard Operating Procedures",
    subtitle = "Biogeochemistry Lab, Pomona College"
  ),

  # Field SOPs only (F-series)
  list(
    filter   = function(e) grepl("^SOP-F", e$Number),
    output   = file.path(output_dir, "EA-Program-Field-SOPs.pdf"),
    title    = "Field Standard Operating Procedures",
    subtitle = "Sampling, Monitoring, and Site Work"
  ),

  # Lab SOPs only (L-series)
  list(
    filter   = function(e) grepl("^SOP-L", e$Number),
    output   = file.path(output_dir, "EA-Program-Lab-SOPs.pdf"),
    title    = "Laboratory Standard Operating Procedures",
    subtitle = "Analysis, Instrumentation, and Quality Control"
  ),

  # Safety and admin (L001--L099)
  list(
    filter   = function(e) {
      grepl("^SOP-L0[0-9]{2}$", e$Number)
    },
    output   = file.path(output_dir, "EA-Program-Safety-SOPs.pdf"),
    title    = "Safety and Administration",
    subtitle = "Laboratory Safety, Waste, and Training"
  ),

  # Molecular biology (L700--L799 and F700--F799)
  list(
    filter   = function(e) {
      grepl("^SOP-[LF]7[0-9]{2}$", e$Number)
    },
    output   = file.path(output_dir, "EA-Program-Molecular-SOPs.pdf"),
    title    = "Molecular Biology Procedures",
    subtitle = "eDNA, PCR, Sequencing, and Bioinformatics"
  )
)

# =============================================================================
# 4. Build each set
# =============================================================================
for (s in sets) {
  mask    <- s$filter(all_entries)
  subset  <- all_entries[mask, ]
  build_combined_pdf(subset, s$output, s$title, s$subtitle)
}

cat("\n=== All combined PDFs built ===\n")
cat("Output directory:", output_dir, "\n")
cat("Run 'Rscript dustjackets.R --prepend' to add dust jackets.\n")
