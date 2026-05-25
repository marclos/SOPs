#!/usr/bin/env Rscript
# =============================================================================
# merge_sop_pdfs_by_topic.R
# Split rendered SOP PDFs into topic-based subsets, each with its own
# hyperlinked TOC cover page.
#
# Groups (by the numeric portion of the SOP number):
#   1. Safety and Administration                     (001--099)
#   2. Equipment and Instrumentation                 (100--299)
#   3. Water and Aquatic Methods                     (300--399)
#   4. Soil Methods                                  (400--499)
#   5. Air and Atmospheric Methods                   (500--599)
#   6. Biology, Ecology, and Molecular Biology       (600--799)
#   7. Data Management and Analysis                  (800--899)
#
# Field (F) and Lab (L) SOPs are combined within each group.
# Only groups containing at least one SOP produce a PDF.
#
# Each SOP is padded (if necessary) so that the next SOP always begins
# on an odd-numbered page -- i.e. a fresh front side when duplex printing.
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
#   docs/EA-SOPs-Safety-Admin.pdf
#   docs/EA-SOPs-Equipment-Instrumentation.pdf
#   docs/EA-SOPs-Water.pdf
#   docs/EA-SOPs-Soil.pdf
#   docs/EA-SOPs-Air.pdf
#   docs/EA-SOPs-Biology-MolBio.pdf
#   docs/EA-SOPs-Data-Management.pdf
# =============================================================================

library(qpdf)

# ---------------------------------------------------------------------------
# 0. Find Pandoc (Quarto bundles its own; rmarkdown needs to know where)
# ---------------------------------------------------------------------------
# If RSTUDIO_PANDOC is already set (e.g. by the CI workflow), honour it.
# Otherwise, look in the typical Quarto install location.
if (Sys.getenv("RSTUDIO_PANDOC") == "") {
  quarto_pandoc <- Sys.which("quarto")
  if (nzchar(quarto_pandoc)) {
    quarto_prefix <- system2("quarto", "--paths", stdout = TRUE)[1]
    pandoc_candidates <- list.files(
      quarto_prefix, pattern = "^pandoc$",
      full.names = TRUE, recursive = TRUE
    )
    if (length(pandoc_candidates) >= 1) {
      Sys.setenv(RSTUDIO_PANDOC = dirname(pandoc_candidates[1]))
      cat("Set RSTUDIO_PANDOC to:", dirname(pandoc_candidates[1]), "\n")
    }
  }
}

\makeatletter
\newcommand{\startodddpage}{%
  \clearpage
  \ifodd\value{page}\else
    \null\thispagestyle{empty}\clearpage
  \fi
}
\makeatother

# ---------------------------------------------------------------------------
# 1. Locate the individual SOP PDFs
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
pdf_dir <- if (length(args) >= 1) args[1] else "docs/pdfs"

if (!dir.exists(pdf_dir)) {
  stop("PDF directory not found: ", pdf_dir,
       "\nRun 'quarto render' first, or pass the correct path.")
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
  if (num >= 1   && num <= 99)  return("safety_admin")
  if (num >= 100 && num <= 299) return("equip_instruments")
  if (num >= 300 && num <= 399) return("water")
  if (num >= 400 && num <= 499) return("soil")
  if (num >= 500 && num <= 599) return("air")
  if (num >= 600 && num <= 799) return("bio_molbio")
  if (num >= 800 && num <= 899) return("data_mgmt")
  "other"
}

groups <- vapply(sop_nums_int, assign_group, character(1))

# ---------------------------------------------------------------------------
# 4. Define group metadata
# ---------------------------------------------------------------------------
group_info <- list(
  safety_admin = list(
    title    = "Safety and Administration",
    subtitle = "General safety, field safety, lab safety, and administrative protocols",
    filename = "EA-SOPs-Safety-Admin.pdf"
  ),
  equip_instruments = list(
    title    = "Equipment and Instrumentation",
    subtitle = "Lab and field equipment operation, analytical instrument procedures, and calibration",
    filename = "EA-SOPs-Equipment-Instrumentation.pdf"
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
  ),
  data_mgmt = list(
    title    = "Data Management and Analysis",
    subtitle = "RStudio, GitHub, QC/QA, GIS, statistics, and instrument data workflows",
    filename = "EA-SOPs-Data-Management.pdf"
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
#    Each SOP is padded with a blank page when needed so the next SOP
#    starts on an odd (front) page for duplex printing.
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

  # Estimate the number of TOC pages (conservative: ~38 rows per page)
  toc_pages <- ceiling(n / 38)

  # Track the running page total so we know when to insert blank pages.
  # After the TOC + \newpage we have consumed toc_pages pages.
  running_pages <- toc_pages

  # Build includepdf lines with odd-page padding
  includepdf_lines <- vapply(seq_along(pdf_abs), function(i) {
    anchor_cmd <- sprintf("\\hypertarget{%s}{}", anchors[i])
    
    # Force odd page start -- LaTeX handles the actual page count
    start_cmd <- "\\startodddpage\n"
    
    if (pages[i] == 1L) {
      block <- sprintf("\\includepdf[pages=-, pagecommand={%s}]{%s}",
                       anchor_cmd, pdf_abs[i])
    } else {
      block <- sprintf(
        "\\includepdf[pages=1, pagecommand={%s}]{%s}\n\\includepdf[pages=2-, pagecommand={}]{%s}",
        anchor_cmd, pdf_abs[i], pdf_abs[i]
      )
    }
    
    paste0(start_cmd, block)
  }, character(1))

  # -- Assemble LaTeX master --
  output_dir <- dirname(pdf_dir)
  output_file <- file.path(output_dir, info$filename)

  master_tex <- paste0(
'\\documentclass[letterpaper]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{pdfpages}
\\usepackage{longtable}
\\usepackage[table]{xcolor}
\\usepackage{hyperref}
\\hypersetup{colorlinks=true, linkcolor=blue, urlcolor=blue}
\\pagestyle{empty}
\\begin{document}

\\begin{center}
{\\LARGE\\bfseries ', escape_latex(info$title), '}\\\\[6pt]
{\\large ', escape_latex(info$subtitle), '}\\\\[4pt]
{\\small EA Program -- Biogeochemistry Lab, Pomona College}\\\\[2pt]
{\\small Compiled: \\today}
\\end{center}

\\vspace{6pt}

', toc_table, '

\\startodddpage

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
              output_file, nrow(data.frame(idx)), total_pages))

  invisible(output_file)
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
