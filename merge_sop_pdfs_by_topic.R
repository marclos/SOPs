#!/usr/bin/env Rscript
# =============================================================================
# merge_sop_pdfs_by_topic.R
# Split rendered SOP PDFs into topic-based subsets, each with its own
# hyperlinked TOC cover page.
#
# Groups (by the numeric portion of the SOP number):
#   1. Safety, Equipment, Instrumentation, and Data  (001--299, 800--899)
#   2. Water and Aquatic Methods                     (300--399)
#   3. Soil Methods                                  (400--499)
#   4. Air and Atmospheric Methods                   (500--599)
#   5. Biology, Ecology, and Molecular Biology       (600--799)
#
# Field (F) and Lab (L) SOPs are combined within each group.
# Only groups containing at least one SOP produce a PDF.
#
# Usage (after quarto render):
#   Rscript merge_sop_pdfs_by_topic.R              # uses docs/pdfs/
#   Rscript merge_sop_pdfs_by_topic.R path/to/pdfs  # custom input directory
#
# Requirements (same as merge_sop_pdfs.R):
#   install.packages(c("qpdf", "rmarkdown", "tinytex"))
#   TinyTeX or a LaTeX distribution with: pdfpages, longtable, xcolor,
#     geometry, hyperref
#
# Output:
#   docs/EA-SOPs-Safety-Equipment-Data.pdf
#   docs/EA-SOPs-Water.pdf
#   docs/EA-SOPs-Soil.pdf
#   docs/EA-SOPs-Air.pdf
#   docs/EA-SOPs-Biology-MolBio.pdf
# =============================================================================

library(qpdf)

# ---------------------------------------------------------------------------
# 0. Find Pandoc (Quarto bundles its own; rmarkdown needs to know where)
# ---------------------------------------------------------------------------
quarto_pandoc <- Sys.which("quarto")
if (!nzchar(Sys.which("pandoc"))) {
  rstudio_pandoc <- Sys.getenv("RSTUDIO_PANDOC", "")
  if (nzchar(rstudio_pandoc) && file.exists(file.path(rstudio_pandoc, "pandoc"))) {
    cat(sprintf("Using Pandoc from RSTUDIO_PANDOC: %s\n", rstudio_pandoc))
  } else {
    pandoc_candidates <- c(
      file.path(rstudio_pandoc, "pandoc"),
      file.path(Sys.getenv("HOME"), ".local", "bin", "pandoc"),
      "/opt/quarto/bin/tools/x86_64/pandoc",
      "/usr/local/bin/pandoc",
      "/usr/bin/pandoc"
    )
    found <- pandoc_candidates[file.exists(pandoc_candidates)]
    if (length(found) > 0) {
      Sys.setenv(RSTUDIO_PANDOC = dirname(found[1]))
    } else {
      stop("Pandoc not found. Install Pandoc or Quarto and ensure it is on PATH.")
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

# ---------------------------------------------------------------------------
# 1. Locate the individual PDFs
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
pdf_dir <- if (length(args) >= 1) args[1] else "docs/pdfs"

if (!dir.exists(pdf_dir)) {
  stop("PDF directory not found: ", pdf_dir,
       "\nRun 'quarto render' first, then try again.")
}

pdf_files <- sort(list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE))

if (length(pdf_files) == 0) {
  stop("No PDF files found in ", pdf_dir)
}

cat(sprintf("Found %d SOP PDFs in %s\n", length(pdf_files), pdf_dir))

# ---------------------------------------------------------------------------
# 2. Read metadata from .qmd YAML for each PDF
# ---------------------------------------------------------------------------
page_counts <- vapply(pdf_files, function(f) {
  length(qpdf::pdf_length(f))
}, integer(1))

find_qmd <- function(pdf_path) {
  stem <- tools::file_path_sans_ext(basename(pdf_path))
  qmd_candidates <- list.files(
    c("lab_sops", "field_sops", "sops"),
    pattern = paste0("^", stem, "\\.qmd$"),
    full.names = TRUE, recursive = FALSE
  )
  if (length(qmd_candidates) >= 1) qmd_candidates[1] else NA_character_
}

titles <- vapply(pdf_files, function(f) {
  qmd <- find_qmd(f)
  if (!is.na(qmd)) {
    yaml_front <- rmarkdown::yaml_front_matter(qmd)
    if (!is.null(yaml_front$title)) return(yaml_front$title)
  }
  tools::file_path_sans_ext(basename(f))
}, character(1))

sop_numbers <- vapply(pdf_files, function(f) {
  qmd <- find_qmd(f)
  if (!is.na(qmd)) {
    yaml_front <- rmarkdown::yaml_front_matter(qmd)
    if (!is.null(yaml_front[["sop-number"]])) return(yaml_front[["sop-number"]])
  }
  tools::file_path_sans_ext(basename(f))
}, character(1))

# ---------------------------------------------------------------------------
# 3. Extract the numeric portion from SOP numbers and assign groups
# ---------------------------------------------------------------------------
# SOP numbers look like "SOP-F001", "SOP-L305", "SOP-F620", etc.
# We extract the trailing digits to classify by range.

extract_num <- function(sop_num) {
  m <- regmatches(sop_num, regexpr("[0-9]+$", sop_num))
  if (length(m) == 1L) as.integer(m) else NA_integer_
}

sop_nums_int <- vapply(sop_numbers, extract_num, integer(1))

assign_group <- function(num) {
  if (is.na(num)) return("other")
  if (num >= 1   && num <= 299) return("safety_equip_data")
  if (num >= 300 && num <= 399) return("water")
  if (num >= 400 && num <= 499) return("soil")
  if (num >= 500 && num <= 599) return("air")
  if (num >= 600 && num <= 799) return("bio_molbio")
  if (num >= 800 && num <= 899) return("safety_equip_data")
  "other"
}

groups <- vapply(sop_nums_int, assign_group, character(1))

# ---------------------------------------------------------------------------
# 4. Define group metadata
# ---------------------------------------------------------------------------
group_info <- list(
  safety_equip_data = list(
    title    = "Safety, Equipment, Instrumentation, and Data Management",
    subtitle = "General safety, lab and field equipment, instrument operation, QC, and data workflows",
    filename = "EA-SOPs-Safety-Equipment-Data.pdf"
  ),
  water = list(
    title    = "Water and Aquatic Methods",
    subtitle = "Field sampling and laboratory analysis for water quality and aquatic systems",
    filename = "EA-SOPs-Water.pdf"
  ),
  soil = list(
    title    = "Soil Methods",
    subtitle = "Field sampling and laboratory analysis for soils and sediments",
    filename = "EA-SOPs-Soil.pdf"
  ),
  air = list(
    title    = "Air and Atmospheric Methods",
    subtitle = "Field monitoring and laboratory analysis for air quality and atmospheric measurements",
    filename = "EA-SOPs-Air.pdf"
  ),
  bio_molbio = list(
    title    = "Biology, Ecology, and Molecular Biology",
    subtitle = "Biodiversity surveys, organism sampling, DNA extraction, sequencing, and bioinformatics",
    filename = "EA-SOPs-Biology-MolBio.pdf"
  )
)

# ---------------------------------------------------------------------------
# 5. Helper functions (shared with merge_sop_pdfs.R)
# ---------------------------------------------------------------------------
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
# 6. Build and compile one PDF per group
# ---------------------------------------------------------------------------
build_group_pdf <- function(group_key, idx) {
  info <- group_info[[group_key]]
  n <- length(idx)

  cat(sprintf("\n--- %s (%d SOPs) ---\n", info$title, n))

  g_numbers    <- sop_numbers[idx]
  g_titles     <- titles[idx]
  g_pages      <- page_counts[idx]
  g_pdf_abs    <- normalizePath(pdf_files[idx])
  g_anchors    <- make_anchor(g_numbers)

  # TOC table
  toc_lines <- sprintf(
    "  \\hyperlink{%s}{%s} & %s \\\\",
    g_anchors,
    escape_latex(g_numbers),
    escape_latex(g_titles)
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

  # includepdf blocks
  includepdf_lines <- vapply(seq_len(n), function(i) {
    np <- g_pages[i]
    anchor_cmd <- sprintf("\\hypertarget{%s}{}", g_anchors[i])
    if (np == 1L) {
      sprintf("\\includepdf[pages=-, pagecommand={%s}]{%s}",
              anchor_cmd, g_pdf_abs[i])
    } else {
      sprintf(
        "\\includepdf[pages=1, pagecommand={%s}]{%s}\n\\includepdf[pages=2-, pagecommand={}]{%s}",
        anchor_cmd, g_pdf_abs[i], g_pdf_abs[i]
      )
    }
  }, character(1))

  # LaTeX document
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
  {\\Large\\textbf{\\textcolor{eablue}{EA Program -- Standard Operating Procedures}}}\\\\[6pt]
  {\\large\\textbf{', escape_latex(info$title), '}}\\\\[4pt]
  {\\normalsize Biogeochemistry Lab, Pomona College}\\\\[4pt]
  {\\small\\textit{', escape_latex(info$subtitle), '}}\\\\[4pt]
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

  # Compile
  build_dir <- tempdir()
  tex_file  <- file.path(build_dir, paste0(
    tools::file_path_sans_ext(info$filename), ".tex"
  ))
  writeLines(master_tex, tex_file)

  cat("  Compiling...\n")
  compiled_pdf <- tinytex::latexmk(tex_file, engine = "pdflatex", clean = TRUE)

  # Move to output
  output_file <- file.path(dirname(pdf_dir), info$filename)
  file.copy(compiled_pdf, output_file, overwrite = TRUE)

  total_pages <- length(qpdf::pdf_length(output_file))
  cat(sprintf("  Done: %s (%d SOPs, %d pages)\n",
              output_file, n, total_pages))

  output_file
}

# ---------------------------------------------------------------------------
# 7. Iterate over groups
# ---------------------------------------------------------------------------
active_groups <- unique(groups)
active_groups <- active_groups[active_groups != "other"]

# Report any unclassified SOPs
other_idx <- which(groups == "other")
if (length(other_idx) > 0) {
  cat("\nNote: the following SOPs did not match any topic group",
      "and will not appear in the subset PDFs:\n")
  for (i in other_idx) {
    cat(sprintf("  %s  %s\n", sop_numbers[i], titles[i]))
  }
}

results <- list()
for (gk in names(group_info)) {
  idx <- which(groups == gk)
  if (length(idx) == 0) {
    cat(sprintf("\nSkipping '%s' -- no SOPs in this range.\n",
                group_info[[gk]]$title))
    next
  }
  results[[gk]] <- build_group_pdf(gk, idx)
}

# ---------------------------------------------------------------------------
# 8. Summary
# ---------------------------------------------------------------------------
cat("\n===== Summary =====\n")
cat(sprintf("Total SOPs processed: %d\n", sum(groups != "other")))
if (length(other_idx) > 0) {
  cat(sprintf("Unclassified SOPs:    %d\n", length(other_idx)))
}
cat(sprintf("PDFs produced:        %d\n", length(results)))
for (f in results) {
  cat(sprintf("  %s\n", f))
}
cat("\nEach PDF has a hyperlinked TOC. Individual SOPs retain their own pagination.\n")
