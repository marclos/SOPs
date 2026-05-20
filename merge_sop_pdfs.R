#!/usr/bin/env Rscript
# =============================================================================
# merge_sop_pdfs.R
# Merge all rendered SOP PDFs into a single document with a TOC cover page.
#
# Usage (after quarto render):
#   Rscript merge_sop_pdfs.R              # uses docs/pdfs/ by default
#   Rscript merge_sop_pdfs.R path/to/pdfs  # custom input directory
#
# Requirements:
#   install.packages(c("qpdf", "pdftools", "rmarkdown"))
#   TinyTeX or a LaTeX distribution (for the TOC page)
#
# Output:
#   docs/EA-Program-SOPs-Complete.pdf
# =============================================================================

library(qpdf)

# ---------------------------------------------------------------------------
# 0. Find Pandoc (Quarto bundles its own; rmarkdown needs to know where)
# ---------------------------------------------------------------------------
# Try quarto's pandoc first, then fall back to PATH
quarto_pandoc <- Sys.which("quarto")
if (nzchar(quarto_pandoc)) {
  quarto_bin <- tryCatch(
    system2("quarto", "--paths", stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  # quarto --paths returns: line 1 = quarto share, line 2 = pandoc dir (if present)
  pandoc_candidates <- c(
    file.path(dirname(quarto_pandoc), "tools"),
    if (length(quarto_bin) >= 1) file.path(quarto_bin[1], "bin", "tools"),
    dirname(quarto_pandoc)
  )
  for (p in pandoc_candidates) {
    if (file.exists(file.path(p, "pandoc")) || file.exists(file.path(p, "pandoc.exe"))) {
      Sys.setenv(RSTUDIO_PANDOC = p)
      cat(sprintf("Using Pandoc from: %s\n", p))
      break
    }
  }
}

# Last resort: search common CI locations
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
  # Find pandoc anywhere on the system
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
# 2. Build a TOC by reading each PDF's page count + metadata from .qmd YAML
# ---------------------------------------------------------------------------
page_counts <- vapply(pdf_files, function(f) {
  length(qpdf::pdf_length(f))
}, integer(1))

# Helper: find the .qmd source that corresponds to a given PDF
find_qmd <- function(pdf_path) {
  stem <- tools::file_path_sans_ext(basename(pdf_path))
  qmd_candidates <- list.files(
    c("lab_sops", "field_sops", "sops"),
    pattern = paste0("^", stem, "\\.qmd$"),
    full.names = TRUE, recursive = FALSE
  )
  if (length(qmd_candidates) >= 1) qmd_candidates[1] else NA_character_
}

# Extract titles and SOP numbers from YAML front matter
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

# Page 1 = TOC; first SOP starts on page 2
start_page <- cumsum(c(2L, head(page_counts, -1)))

toc_entries <- data.frame(
  Number = sop_numbers,
  SOP    = titles,
  Pages  = page_counts,
  Start  = start_page,
  stringsAsFactors = FALSE
)

cat("\nTable of Contents:\n")
print(toc_entries, row.names = FALSE)

# ---------------------------------------------------------------------------
# 3. Render a TOC cover page to PDF via a temporary .Rmd
# ---------------------------------------------------------------------------

# Escape any LaTeX special characters in titles and SOP numbers
escape_latex <- function(x) {
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("\\$", "\\$", x, fixed = TRUE)
  x <- gsub("#", "\\#", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x <- gsub("\\{", "\\{", x, fixed = TRUE)
  x <- gsub("\\}", "\\}", x, fixed = TRUE)
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\textasciicircum{}", x, fixed = TRUE)
  x
}

# Build LaTeX tabularx rows
toc_lines <- sprintf("  %s & %s & %d \\\\",
                     escape_latex(toc_entries$Number),
                     escape_latex(toc_entries$SOP),
                     toc_entries$Start)

toc_table <- paste(
  "\\begin{tabularx}{\\textwidth}{|p{2.2cm}|X|r|}",
  "\\hline",
  "\\textbf{SOP Number} & \\textbf{Title} & \\textbf{Page} \\\\",
  "\\hline",
  paste(toc_lines, collapse = "\n  \\hline\n"),
  "\\hline",
  "\\end{tabularx}",
  sep = "\n"
)

toc_rmd_content <- paste0(
'---
title: "EA Program -- Standard Operating Procedures"
subtitle: "Biogeochemistry Lab, Pomona College"
date: "', format(Sys.Date(), "%B %d, %Y"), '"
output:
  pdf_document:
    latex_engine: pdflatex
header-includes:
  - \\usepackage{tabularx}
geometry: margin=1in
fontsize: 11pt
---

# Table of Contents

', toc_table, '

\\vfill

*This document was generated automatically by `merge_sop_pdfs.R`.*
')

toc_rmd <- tempfile(fileext = ".Rmd")
toc_pdf <- tempfile(fileext = ".pdf")
writeLines(toc_rmd_content, toc_rmd)

cat("\nRendering TOC cover page...\n")
rmarkdown::render(toc_rmd, output_file = toc_pdf, quiet = TRUE)

# ---------------------------------------------------------------------------
# 4. Merge TOC + all SOP PDFs
# ---------------------------------------------------------------------------
output_file <- file.path(dirname(pdf_dir), "EA-Program-SOPs-Complete.pdf")

all_pdfs <- c(toc_pdf, pdf_files)

cat("Merging into", output_file, "...\n")
qpdf::pdf_combine(all_pdfs, output = output_file)

cat(sprintf(
  "Done. Combined PDF: %s (%d SOPs, %d total pages)\n",
  output_file,
  length(pdf_files),
  sum(page_counts) + length(qpdf::pdf_length(toc_pdf))
))

# Cleanup temp files
unlink(c(toc_rmd, toc_pdf))
