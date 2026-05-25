#!/usr/bin/env Rscript
# =============================================================================
# dustjackets.R
# =============================================================================
# Generates standalone 2-page dust jacket PDFs (cover + spine) for each
# SOP binder, then optionally prepends them to the corresponding compiled
# PDF using qpdf.
#
# Usage:
#   Rscript dustjackets.R              # build all dust jackets
#   Rscript dustjackets.R --prepend    # build and prepend to target PDFs
#
# The jacket list below must stay aligned with the PDFs produced by:
#   - merge_sop_pdfs.R           -> EA-Program-SOPs-Complete.pdf
#   - merge_sop_pdfs_by_topic.R  -> EA-SOPs-Safety-Admin.pdf,
#                                   EA-SOPs-Equipment-Instrumentation.pdf,
#                                   EA-SOPs-Water.pdf, EA-SOPs-Soil.pdf, etc.
#
# Each jacket is defined as a list entry below. To add a new binder,
# just add another entry to the `jackets` list.
#
# Prerequisites:
#   - TinyTeX or a full LaTeX distribution
#   - R packages: tinytex, qpdf
#   - The image files referenced in each jacket must exist
#   - The template _templates/dustjacket.tex must exist
# =============================================================================

library(tinytex)

# ---------------------------------------------------------------------------
# 1. Define the dust jackets
# ---------------------------------------------------------------------------

jackets <- list(

  list(
    name        = "dustjacket-complete",
    title       = "STANDARD OPERATING PROCEDURES",
    subtitle    = "Complete Collection",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "STANDARD OPERATING PROCEDURES BINDER",
    target_pdf  = "docs/EA-Program-SOPs-Complete.pdf"
  ),

  list(
    name        = "dustjacket-safety-admin",
    title       = "SAFETY AND ADMINISTRATION",
    subtitle    = "General Safety, Lab Safety, Field Safety, and Administrative Protocols",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "SAFETY AND ADMINISTRATION",
    target_pdf  = "docs/EA-SOPs-Safety-Admin.pdf"
  ),

  list(
    name        = "dustjacket-equip-instruments",
    title       = "EQUIPMENT AND INSTRUMENTATION",
    subtitle    = "Lab and Field Equipment, Analytical Instruments, and Calibration",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "EQUIPMENT AND INSTRUMENTATION",
    target_pdf  = "docs/EA-SOPs-Equipment-Instrumentation.pdf"
  ),

  list(
    name        = "dustjacket-water",
    title       = "WATER AND AQUATIC METHODS",
    subtitle    = "Field Sampling and Laboratory Analysis for Water Quality",
    image       = "images/dustjacket-water",
    image_width = "5.0in",
    spine       = "WATER AND AQUATIC METHODS",
    target_pdf  = "docs/EA-SOPs-Water.pdf"
  ),

  list(
    name        = "dustjacket-soil",
    title       = "SOIL METHODS",
    subtitle    = "Field Sampling and Laboratory Analysis for Soils and Sediments",
    image       = "images/dustjacket-soil",
    image_width = "5.0in",
    spine       = "SOIL METHODS",
    target_pdf  = "docs/EA-SOPs-Soil.pdf"
  ),

  list(
    name        = "dustjacket-air",
    title       = "AIR AND ATMOSPHERIC METHODS",
    subtitle    = "Field Monitoring and Laboratory Analysis for Air Quality",
    image       = "images/dustjacket-air",
    image_width = "5.0in",
    spine       = "AIR AND ATMOSPHERIC METHODS",
    target_pdf  = "docs/EA-SOPs-Air.pdf"
  ),

  list(
    name        = "dustjacket-bio-molbio",
    title       = "BIOLOGY AND MOLECULAR BIOLOGY",
    subtitle    = "Biodiversity Surveys, DNA Extraction, Sequencing, and Bioinformatics",
    image       = "images/dustjacket-edna",
    image_width = "5.0in",
    spine       = "BIOLOGY AND MOLECULAR BIOLOGY",
    target_pdf  = "docs/EA-SOPs-Biology-MolBio.pdf"
  ),

  list(
    name        = "dustjacket-data-mgmt",
    title       = "DATA MANAGEMENT AND ANALYSIS",
    subtitle    = "RStudio, GitHub, QC/QA, GIS, Statistics, and Data Workflows",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "DATA MANAGEMENT AND ANALYSIS",
    target_pdf  = "docs/EA-SOPs-Data-Management.pdf"
  )
)

# ---------------------------------------------------------------------------
# 2. Check for --prepend flag
# ---------------------------------------------------------------------------
do_prepend <- "--prepend" %in% commandArgs(trailingOnly = TRUE)

# ---------------------------------------------------------------------------
# 3. Build each dust jacket
# ---------------------------------------------------------------------------
jacket_dir <- "docs/dustjackets"
if (!dir.exists(jacket_dir)) dir.create(jacket_dir, recursive = TRUE)

template_path <- "_templates/dustjacket.tex"
if (!file.exists(template_path)) {
  stop("Template not found: ", template_path,
       "\nPlace dustjacket.tex in _templates/")
}

# Save project root -- all paths are relative to here
project_root <- getwd()

for (j in jackets) {

  cat(sprintf("\n--- Building: %s ---\n", j$name))

  escape_tex <- function(x) gsub("&", "\\\\&", x)

  # Build the standalone LaTeX document.
  # \newcommand defines the variables that _templates/dustjacket.tex uses.
  # \input brings in the template (which uses \djTitle, \djSubtitle, etc.).
  tex_content <- sprintf(
'\\documentclass[11pt,letterpaper]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{graphicx}
\\usepackage{rotating}
\\pagestyle{empty}

%%%% Define jacket parameters
\\newcommand{\\djTitle}{%s}
\\newcommand{\\djSubtitle}{%s}
\\newcommand{\\djImage}{%s}
\\newcommand{\\djImageWidth}{%s}
\\newcommand{\\djSpineText}{%s}

\\begin{document}
\\input{%s}
\\end{document}
',
    escape_tex(j$title),
    escape_tex(j$subtitle),
    j$image,
    j$image_width,
    escape_tex(j$spine),
    template_path
  )

  # Write the .tex file in the PROJECT ROOT so that relative paths resolve:
  #   - \input{_templates/dustjacket.tex}
  #   - \includegraphics{images/Hazard_sign}
  tex_file <- file.path(project_root, paste0(j$name, ".tex"))
  writeLines(tex_content, tex_file)

  # Ensure we compile from the project root
  setwd(project_root)

  compiled <- tryCatch(
    tinytex::latexmk(tex_file, engine = "pdflatex", clean = TRUE),
    error = function(e) {
      warning(sprintf("Failed to compile %s: %s", j$name, e$message))
      return(NULL)
    }
  )

  # Always return to project root after compilation
  setwd(project_root)

  if (is.null(compiled)) next

  # Move to output directory
  out_pdf <- file.path(jacket_dir, paste0(j$name, ".pdf"))
  file.copy(compiled, out_pdf, overwrite = TRUE)

  # Clean up temp files from project root
  for (ext in c(".tex", ".aux", ".log", ".out")) {
    f <- paste0(j$name, ext)
    if (file.exists(f)) file.remove(f)
  }
  if (file.exists(compiled) && normalizePath(compiled) != normalizePath(out_pdf)) {
    file.remove(compiled)
  }

  cat(sprintf("  -> %s (2 pages)\n", out_pdf))

  # --- Optionally prepend to the target PDF ---
  if (do_prepend && file.exists(j$target_pdf)) {
    cat(sprintf("  Prepending to %s...\n", j$target_pdf))

    combined_tmp <- tempfile(fileext = ".pdf")
    qpdf::pdf_combine(
      input  = c(out_pdf, j$target_pdf),
      output = combined_tmp
    )
    file.copy(combined_tmp, j$target_pdf, overwrite = TRUE)
    file.remove(combined_tmp)

    cat(sprintf("  Done. Jacket prepended to %s\n", j$target_pdf))
  } else if (do_prepend && !file.exists(j$target_pdf)) {
    cat(sprintf("  SKIP: target not found: %s\n", j$target_pdf))
  }
}

setwd(project_root)
cat("\n=== All dust jackets built ===\n")
if (do_prepend) cat("Jackets prepended to target PDFs where available.\n")
