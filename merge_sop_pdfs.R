# =============================================================================
# merge_sop_pdfs.R
# Combine all individual SOP PDFs into a single "Complete" PDF with a
# hyperlinked table of contents.
#
# Topic-based subsets (Water, Soil, Air, etc.) are handled by
# merge_sop_pdfs_by_topic.R -- do NOT define them here.
#
# Each SOP is padded (if necessary) so that the next SOP always begins
# on an odd-numbered page -- i.e. a fresh front side when duplex printing.
#
# Usage:
#   Rscript merge_sop_pdfs.R [pdf_directory]
#   Default pdf_directory: docs/pdfs
# =============================================================================

# ---------------------------------------------------------------------------
# 0. Find Pandoc (Quarto bundles its own; rmarkdown needs to know where)
# ---------------------------------------------------------------------------
# If RSTUDIO_PANDOC is already set (e.g. by the CI workflow), honour it.
if (nzchar(Sys.getenv("RSTUDIO_PANDOC"))) {
  cat(sprintf("Using Pandoc from RSTUDIO_PANDOC: %s\n",
              Sys.getenv("RSTUDIO_PANDOC")))
} else {
  # Try to locate Pandoc via Quarto
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
        cat(sprintf("Using Pandoc from Quarto: %s\n", p))
        break
      }
    }
  }

  # Fallback: common install locations
  if (!rmarkdown::pandoc_available()) {
    common_paths <- c(
      "/opt/quarto/bin/tools",
      "/opt/quarto/bin/tools/x86_64",
      "/usr/local/bin",
      file.path(Sys.getenv("HOME"), ".local", "bin"),
      "/usr/bin",
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

  # Last resort: system PATH
  if (!rmarkdown::pandoc_available()) {
    pandoc_path <- Sys.which("pandoc")
    if (nzchar(pandoc_path)) {
      Sys.setenv(RSTUDIO_PANDOC = dirname(pandoc_path))
      cat(sprintf("Using Pandoc from system PATH: %s\n", dirname(pandoc_path)))
    } else {
      stop("Pandoc not found. Install Pandoc or Quarto and ensure it is on PATH.")
    }
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
#   Each SOP is padded with a blank page when needed so the next SOP
#   starts on an odd (front) page for duplex printing.
# ---------------------------------------------------------------------------
build_combined_pdf <- function(entries, output_file, title, subtitle) {
  cat(sprintf("\nBuilding: %s (%d SOPs)\n", output_file, nrow(entries)))

  numbers  <- entries$Number
  titles   <- entries$SOP
  pages    <- entries$Pages
  pdf_abs  <- normalizePath(entries$file)
  anchors  <- make_anchor(numbers)

  # TOC table
  toc_lines <- sprintf(
    "  \\hyperlink{%s}{%s} & %s \\\\",
    anchors,
    escape_latex(numbers),
    escape_latex(titles)
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

  # -----------------------------------------------------------------------
  # Compute the number of TOC pages so we can track the running total.
  # The TOC is rendered via longtable; we estimate roughly 40 rows per

  # page (each row is ~2 lines with the hline). This is only used to
  # decide whether the TOC itself ends on an odd or even page; a small
  # miscount is harmless because we also pad after the TOC.
  # For safety we round up.
  # -----------------------------------------------------------------------
  n_sops    <- nrow(entries)
  toc_pages <- ceiling(n_sops / 38)  # conservative estimate

  # After the TOC \newpage, the running total = toc_pages.
  # We want the first SOP to start on an odd page, so if toc_pages is
  # already odd we need a blank page to push the first SOP to page
  # toc_pages+2 (even+1 = odd, but the \newpage itself moves us to
  # toc_pages+1).
  # Actually: after the TOC the \newpage moves us to page (toc_pages+1).
  # If (toc_pages+1) is odd, great. If even, insert a blank page.
  # But we handle this more simply: we track a running page total in R
  # and pad after each SOP (including a "virtual" TOC block).

  running_pages <- toc_pages  # pages consumed by the TOC

  # Build includepdf lines with odd-page padding
  includepdf_lines <- vapply(seq_along(pdf_abs), function(i) {
    np <- pages[i]
    anchor_cmd <- sprintf("\\hypertarget{%s}{}", anchors[i])

    # Pad before this SOP if we are currently at an even running total
    # (meaning the next page would be even = back side).
    pad_before <- ""
    if (running_pages %% 2 == 1) {
      # running_pages is odd, so the next page (running_pages+1) is even.
      # Insert a blank page to push the SOP to an odd page.
      pad_before <- "\\null\\thispagestyle{empty}\\newpage\n"
      running_pages <<- running_pages + 1L
    }

    if (np == 1L) {
      block <- sprintf("\\includepdf[pages=-, pagecommand={%s}]{%s}",
                        anchor_cmd, pdf_abs[i])
    } else {
      block <- sprintf(
        "\\includepdf[pages=1, pagecommand={%s}]{%s}\n\\includepdf[pages=2-, pagecommand={}]{%s}",
        anchor_cmd, pdf_abs[i], pdf_abs[i]
      )
    }

    running_pages <<- running_pages + np
    paste0(pad_before, block)
  }, character(1))

  cat(sprintf("  Total pages (with padding): %d\n", running_pages))

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
  {\\Large\\textbf{\\textcolor{eablue}{', escape_latex(title), '}}}\\\\[6pt]
  {\\normalsize ', escape_latex(subtitle), '}\\\\[4pt]
  {\\normalsize ', format(Sys.Date(), "%B %d, %Y"), '}
\\end{center}

\\vspace{12pt}

\\section*{Table of Contents}
\\noindent\\textit{Click any SOP number to jump to that procedure.}

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
# 3. Build ONLY the Complete PDF
# =============================================================================
# Topic-based subsets (Water, Soil, Air, Bio/MolBio, Safety/Equipment)
# are produced by merge_sop_pdfs_by_topic.R -- not here.

output_dir <- dirname(pdf_dir)  # typically "docs"

build_combined_pdf(
  all_entries,
  file.path(output_dir, "EA-Program-SOPs-Complete.pdf"),
  "EA Program -- Standard Operating Procedures",
  "Biogeochemistry Lab, Pomona College"
)

cat("\n=== Complete combined PDF built ===\n")
cat("Output directory:", output_dir, "\n")
cat("Topic subsets are built by merge_sop_pdfs_by_topic.R\n")
