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
# 2. Build a TOC by reading each PDF's page count
# ---------------------------------------------------------------------------
page_counts <- vapply(pdf_files, function(f) {
  length(qpdf::pdf_length(f))
}, integer(1))

# Build cumulative page numbers (the TOC page itself will be page 1)
# ---------------------------------------------------------------------------
# 2. Build a TOC by reading each PDF's page count + titles from .qmd YAML
# ---------------------------------------------------------------------------
page_counts <- vapply(pdf_files, function(f) {
  length(qpdf::pdf_length(f))
}, integer(1))

# Extract human-readable titles from matching .qmd source files
titles <- vapply(pdf_files, function(f) {
  stem <- tools::file_path_sans_ext(basename(f))
  # Search for the .qmd in the directories Quarto renders from
  qmd_candidates <- list.files(
    c("lab_sops", "field_sops", "sops"),
    pattern = paste0("^", stem, "\\.qmd$"),
    full.names = TRUE, recursive = FALSE
  )
  if (length(qmd_candidates) >= 1) {
    yaml_front <- rmarkdown::yaml_front_matter(qmd_candidates[1])
    if (!is.null(yaml_front$title)) return(yaml_front$title)
  }
  # Fallback to filename if no .qmd or no title field
  stem
}, character(1))


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
# Also grab sop-number from YAML
sop_numbers <- vapply(pdf_files, function(f) {
  stem <- tools::file_path_sans_ext(basename(f))
  qmd_candidates <- list.files(
    c("lab_sops", "field_sops", "sops"),
    pattern = paste0("^", stem, "\\.qmd$"),
    full.names = TRUE, recursive = FALSE
  )
  if (length(qmd_candidates) >= 1) {
    yaml_front <- rmarkdown::yaml_front_matter(qmd_candidates[1])
    if (!is.null(yaml_front[["sop-number"]])) return(yaml_front[["sop-number"]])
  }
  stem
}, character(1))

toc_entries <- data.frame(
  Number = sop_numbers,
  SOP    = titles,
  Pages  = page_counts,
  Start  = start_page,
  stringsAsFactors = FALSE
)

# Update the markdown table format
toc_lines <- sprintf("| %s | %s | %d |", toc_entries$Number, toc_entries$SOP, toc_entries$Start)
toc_table <- paste(
  "| SOP Number | Title | Page |",
  "|:-----------|:------|-----:|",
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
