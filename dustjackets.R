# =============================================================================
# dustjackets.R
# Generate binder dust-jacket PDFs (cover + spine) and optionally prepend
# them to the corresponding compiled SOP PDFs.
#
# Usage:
#   Rscript dustjackets.R              # Generate standalone jacket PDFs only
#   Rscript dustjackets.R --prepend    # Generate and prepend to target PDFs
#
# The jacket list below must stay aligned with the PDFs produced by:
#   - merge_sop_pdfs.R           -> EA-Program-SOPs-Complete.pdf
#   - merge_sop_pdfs_by_topic.R  -> EA-SOPs-Water.pdf, EA-SOPs-Soil.pdf, etc.
#
# To add a new binder, just add another entry to the `jackets` list.
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
# Each entry produces one 2-page PDF.
#
#   name       : output filename (without .pdf)
#   title      : large title on the cover
#   subtitle   : line below the title (binder description)
#   image      : path to the cover image (relative to project root)
#   image_width: LaTeX width for the image
#   spine      : text for the rotated spine page
#   target_pdf : the compiled PDF to prepend this jacket to (used with --prepend)

jackets <- list(

  # ---- Complete collection ----
  list(
    name        = "dustjacket-complete",
    title       = "STANDARD OPERATING PROCEDURES",
    subtitle    = "Complete Collection",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "STANDARD OPERATING PROCEDURES BINDER",
    target_pdf  = "docs/EA-Program-SOPs-Complete.pdf"
  ),

  # ---- Topic: Safety, Equipment, Instrumentation, and Data Management ----
  list(
    name        = "dustjacket-safety-equip-data",
    title       = "SAFETY, EQUIPMENT, AND DATA MANAGEMENT",
    subtitle    = "General Safety, Lab and Field Equipment, Instrument Operation, and Data Workflows",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "SAFETY / EQUIPMENT / DATA MANAGEMENT",
    target_pdf  = "docs/EA-SOPs-Safety-Equipment-Data.pdf"
  ),

  # ---- Topic: Water and Aquatic Methods ----
  list(
    name        = "dustjacket-water",
    title       = "WATER AND AQUATIC METHODS",
    subtitle    = "Field Sampling and Laboratory Analysis for Water Quality and Aquatic Systems",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "WATER AND AQUATIC METHODS",
    target_pdf  = "docs/EA-SOPs-Water.pdf"
  ),

  # ---- Topic: Soil Methods ----
  list(
    name        = "dustjacket-soil",
    title       = "SOIL METHODS",
    subtitle    = "Field Sampling and Laboratory Analysis for Soils and Sediments",
    image       = "images/soil-cover",
    image_width = "5.0in",
    spine       = "SOIL METHODS",
    target_pdf  = "docs/EA-SOPs-Soil.pdf"
  ),

  # ---- Topic: Air and Atmospheric Methods ----
  list(
    name        = "dustjacket-air",
    title       = "AIR AND ATMOSPHERIC METHODS",
    subtitle    = "Field Monitoring and Laboratory Analysis for Air Quality",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "AIR AND ATMOSPHERIC METHODS",
    target_pdf  = "docs/EA-SOPs-Air.pdf"
  ),

  # ---- Topic: Biology, Ecology, and Molecular Biology ----
  list(
    name        = "dustjacket-bio-molbio",
    title       = "BIOLOGY AND MOLECULAR BIOLOGY",
    subtitle    = "Biodiversity Surveys, Organism Sampling, DNA Extraction, Sequencing, and Bioinformatics",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "BIOLOGY AND MOLECULAR BIOLOGY",
    target_pdf  = "docs/EA-SOPs-Biology-MolBio.pdf"
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
# 3. Build each dust jacket PDF
# ---------------------------------------------------------------------------
build_dir <- file.path(tempdir(), "dustjackets")
dir.create(build_dir, showWarnings = FALSE, recursive = TRUE)

jacket_pdfs <- list()

for (j in jackets) {
  cat(sprintf("Building jacket: %s\n", j$name))

  tex_content <- template
  tex_content <- gsub("<<TITLE>>",       j$title,       tex_content, fixed = TRUE)
  tex_content <- gsub("<<SUBTITLE>>",    j$subtitle,    tex_content, fixed = TRUE)
  tex_content <- gsub("<<IMAGE>>",       j$image,       tex_content, fixed = TRUE)
  tex_content <- gsub("<<IMAGE_WIDTH>>", j$image_width, tex_content, fixed = TRUE)
  tex_content <- gsub("<<SPINE>>",       j$spine,       tex_content, fixed = TRUE)
  tex_content <- gsub("<<DATE>>",        format(Sys.Date(), "%B %d, %Y"),
                       tex_content, fixed = TRUE)

  tex_file <- file.path(build_dir, paste0(j$name, ".tex"))
  writeLines(tex_content, tex_file)

  compiled <- tinytex::latexmk(tex_file, engine = "pdflatex", clean = TRUE)

  output_path <- file.path("docs", paste0(j$name, ".pdf"))
  file.copy(compiled, output_path, overwrite = TRUE)
  jacket_pdfs[[j$name]] <- output_path

  cat(sprintf("  -> %s\n", output_path))
}

# ---------------------------------------------------------------------------
# 4. Optionally prepend jackets to target PDFs
# ---------------------------------------------------------------------------
if ("--prepend" %in% commandArgs(trailingOnly = TRUE)) {
  cat("\n--- Prepending dust jackets ---\n")
  for (j in jackets) {
    jacket_file <- jacket_pdfs[[j$name]]
    target_file <- j$target_pdf

    if (!file.exists(target_file)) {
      cat(sprintf("  SKIP: target not found: %s\n", target_file))
      next
    }

    cat(sprintf("  Prepending %s -> %s\n", basename(jacket_file), target_file))

    tmp <- tempfile(fileext = ".pdf")
    qpdf::pdf_combine(c(jacket_file, target_file), tmp)
    file.copy(tmp, target_file, overwrite = TRUE)
    unlink(tmp)
  }
}

cat("\nDone.\n")
