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
# 2. Build a TOC by reading each PDF's page count
# ---------------------------------------------------------------------------
page_counts <- vapply(pdf_files, function(f) {
  length(qpdf::pdf_length(f))
}, integer(1))

# Build cumulative page numbers (the TOC page itself will be page 1)
titles     <- tools::file_path_sans_ext(basename(pdf_files))
start_page <- cumsum(c(2L, head(page_counts, -1)))  
# page 1 = TOC; first SOP starts on page 2

toc_entries <- data.frame(
  SOP   = titles,
  Pages = page_counts,
  Start = start_page,
  stringsAsFactors = FALSE
)

cat("\nTable of Contents:\n")
print(toc_entries, row.names = FALSE)

# ---------------------------------------------------------------------------
# 3. Render a TOC cover page to PDF via a temporary .Rmd
# ---------------------------------------------------------------------------
toc_lines <- sprintf("| %s | %d |", toc_entries$SOP, toc_entries$Start)
toc_table <- paste(
  "| SOP | Page |",
  "|:-----|-----:|",
  paste(toc_lines, collapse = "\n"),
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
