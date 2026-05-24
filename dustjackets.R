#!/usr/bin/env Rscript
# =============================================================================
# dustjackets.R
# Generate dust-jacket cover pages for the combined SOP PDFs and
# optionally prepend them to the target PDFs.
#
# Usage:
#   Rscript dustjackets.R            # generate jacket PDFs only
#   Rscript dustjackets.R --prepend  # generate AND prepend to target PDFs
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
    image       = "images/soil_cover",
    image_width = "5.0in",
    spine       = "SOIL METHODS",
    target_pdf  = "docs/EA-SOPs-Soil.pdf"
  ),

  list(
    name        = "dustjacket-air",
    title       = "AIR AND ATMOSPHERIC METHODS",
    subtitle    = "Field Monitoring and Laboratory Analysis for Air Quality",
    image       = "images/Hazard_sign",
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
# 2. Read the LaTeX template
# ---------------------------------------------------------------------------
template_path <- "_templates/dustjacket.tex"
if (!file.exists(template_path)) {
  stop("Dust jacket template not found: ", template_path)
}
template <- paste(readLines(template_path, warn = FALSE), collapse = "\n")

# ---------------------------------------------------------------------------
# 3. Build each jacket
# ---------------------------------------------------------------------------
prepend <- "--prepend" %in% commandArgs(trailingOnly = TRUE)

for (j in jackets) {
  cat(sprintf("Building jacket: %s\n", j$name))

  tex <- template
  tex <- gsub("<<TITLE>>",       j$title,       tex, fixed = TRUE)
  tex <- gsub("<<SUBTITLE>>",    j$subtitle,    tex, fixed = TRUE)
  tex <- gsub("<<IMAGE>>",       j$image,       tex, fixed = TRUE)
  tex <- gsub("<<IMAGE_WIDTH>>", j$image_width, tex, fixed = TRUE)
  tex <- gsub("<<SPINE>>",       j$spine,       tex, fixed = TRUE)

  build_dir <- tempdir()
  tex_file  <- file.path(build_dir, paste0(j$name, ".tex"))
  writeLines(tex, tex_file)

  jacket_pdf <- tinytex::latexmk(tex_file, engine = "pdflatex",
                                  clean = TRUE)

  # Copy jacket PDF next to target for reference
  jacket_out <- file.path("docs", paste0(j$name, ".pdf"))
  file.copy(jacket_pdf, jacket_out, overwrite = TRUE)
  cat(sprintf("  -> %s\n", jacket_out))

  # Optionally prepend to the target PDF
  if (prepend && file.exists(j$target_pdf)) {
    cat(sprintf("  Prepending to %s\n", j$target_pdf))
    tmp <- tempfile(fileext = ".pdf")
    qpdf::pdf_combine(c(jacket_out, j$target_pdf), output = tmp)
    file.copy(tmp, j$target_pdf, overwrite = TRUE)
    unlink(tmp)
  } else if (prepend && !file.exists(j$target_pdf)) {
    cat(sprintf("  WARNING: target not found, skipping prepend: %s\n",
                j$target_pdf))
  }
}

cat("\nDone.\n")
