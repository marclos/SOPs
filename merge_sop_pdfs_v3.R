#!/usr/bin/env Rscript
# =============================================================================
# merge_sop_pdfs.R
# Merge all rendered SOP PDFs into a single document with a hyperlinked TOC.
#
# Strategy:
#   Build a single LaTeX document that uses \includepdf (pdfpages package) to
#   embed every SOP. Each SOP's first page gets a \hypertarget anchor, and the
#   TOC entries are clickable \hyperlink jumps. No combined-document page
#   numbers are added -- each SOP retains its own internal pagination.
#
# Usage (after quarto render):
#   Rscript merge_sop_pdfs.R              # uses docs/pdfs/ by default
#   Rscript merge_sop_pdfs.R path/to/pdfs  # custom input directory
#
# Requirements:
#   install.packages(c("qpdf", "rmarkdown", "tinytex"))
#   TinyTeX or a LaTeX distribution with: pdfpages, longtable, xcolor,
#     geometry, hyperref, tabularx
#
# Output:
#   docs/EA-Program-SOPs-Complete.pdf
# =============================================================================

library(qpdf)

# ---------------------------------------------------------------------------
# 0. Find Pandoc (Quarto bundles its own; rmarkdown needs to know where)
# ---------------------------------------------------------------------------
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

toc_entries <- data.frame(
  Number = sop_numbers,
  SOP    = titles,
  Pages  = page_counts,
  stringsAsFactors = FALSE
)

cat("\nTable of Contents:\n")
print(toc_entries, row.names = FALSE)

# ---------------------------------------------------------------------------
# 3. Helpers
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

# Create a clean anchor name from SOP number (letters, digits, hyphens only)
make_anchor <- function(x) {
  gsub("[^A-Za-z0-9-]", "", x)
}

# ---------------------------------------------------------------------------
# 4. Build the master LaTeX document
# ---------------------------------------------------------------------------
pdf_abs <- normalizePath(pdf_files)
n <- length(pdf_abs)

anchors <- make_anchor(toc_entries$Number)

# -- TOC table: each row is a clickable hyperlink --------------------------
toc_lines <- sprintf(
  "  \\hyperlink{%s}{%s} & %s \\\\",
  anchors,
  escape_latex(toc_entries$Number),
  escape_latex(toc_entries$SOP)
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

# -- \includepdf blocks: first page of each SOP gets a \hypertarget --------
# pagecommand on page 1 plants the anchor; subsequent pages get no extra
# command (empty pagecommand keeps the SOP's own headers/footers intact).
includepdf_lines <- vapply(seq_len(n), function(i) {
  np <- page_counts[i]
  anchor_cmd <- sprintf("\\hypertarget{%s}{}", anchors[i])
  if (np == 1L) {
    # Single-page SOP
    sprintf("\\includepdf[pages=-, pagecommand={%s}]{%s}",
            anchor_cmd, pdf_abs[i])
  } else {
    # Multi-page: anchor on page 1, nothing extra on the rest
    sprintf("\\includepdf[pages=1, pagecommand={%s}]{%s}\n\\includepdf[pages=2-, pagecommand={}]{%s}",
            anchor_cmd, pdf_abs[i], pdf_abs[i])
  }
}, character(1))

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

% Colors
\\definecolor{eablue}{HTML}{0057B8}

% No headers/footers on the combined document -- each SOP keeps its own
\\pagestyle{empty}

\\begin{document}

% -----------------------------------------------------------------------
% Title / TOC page
% -----------------------------------------------------------------------
\\begin{center}
  {\\Large\\textbf{\\textcolor{eablue}{EA Program -- Standard Operating Procedures}}}\\\\[6pt]
  {\\large Biogeochemistry Lab, Pomona College}\\\\[4pt]
  {\\normalsize ', format(Sys.Date(), "%B %d, %Y"), '}
\\end{center}

\\vspace{12pt}

\\section*{Table of Contents}
\\noindent\\textit{Click any SOP number or title to jump to that procedure.}

\\vspace{6pt}

', toc_table, '

\\newpage

% -----------------------------------------------------------------------
% Include each SOP PDF (hyperlink anchors on first page of each)
% -----------------------------------------------------------------------
', paste(includepdf_lines, collapse = "\n\n"), '

\\end{document}
')

# ---------------------------------------------------------------------------
# 5. Compile the master LaTeX document
# ---------------------------------------------------------------------------
build_dir <- tempdir()
tex_file  <- file.path(build_dir, "EA-Program-SOPs-Complete.tex")
writeLines(master_tex, tex_file)

cat("\nCompiling combined PDF with hyperlinked TOC...\n")

compiled_pdf <- tinytex::latexmk(
  tex_file,
  engine  = "pdflatex",
  clean   = TRUE
)

# ---------------------------------------------------------------------------
# 6. Move to output location
# ---------------------------------------------------------------------------
output_file <- file.path(dirname(pdf_dir), "EA-Program-SOPs-Complete.pdf")
file.copy(compiled_pdf, output_file, overwrite = TRUE)

total_pages <- length(qpdf::pdf_length(output_file))

cat(sprintf(
  "\nDone. Combined PDF: %s (%d SOPs, %d total pages)\n",
  output_file,
  length(pdf_files),
  total_pages
))
cat("TOC entries are hyperlinked to each SOP.\n")
cat("Each SOP retains its own internal page numbering.\n")
