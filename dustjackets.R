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
    name        = "dustjacket-field",
    title       = "FIELD STANDARD OPERATING PROCEDURES",
    subtitle    = "Field Sampling and Monitoring",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "FIELD STANDARD OPERATING PROCEDURES",
    target_pdf  = "docs/EA-Program-Field-SOPs.pdf"
  ),

  list(
    name        = "dustjacket-lab",
    title       = "LABORATORY STANDARD OPERATING PROCEDURES",
    subtitle    = "Laboratory Analysis and Instrumentation",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "LABORATORY STANDARD OPERATING PROCEDURES",
    target_pdf  = "docs/EA-Program-Lab-SOPs.pdf"
  ),

  list(
    name        = "dustjacket-safety",
    title       = "SAFETY AND ADMINISTRATION",
    subtitle    = "Laboratory Safety, Waste, and Training",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "SAFETY AND ADMINISTRATION",
    target_pdf  = "docs/EA-Program-Safety-SOPs.pdf"
  ),

  list(
    name        = "dustjacket-molecular",
    title       = "MOLECULAR BIOLOGY PROCEDURES",
    subtitle    = "eDNA, PCR, and Sequencing",
    image       = "images/Hazard_sign",
    image_width = "5.0in",
    spine       = "MOLECULAR BIOLOGY PROCEDURES",
    target_pdf  = "docs/EA-Program-Molecular-SOPs.pdf"
  )
)

# ---------------------------------------------------------------------------
# 2. Check for --prepend flag
# ---------------------------------------------------------------------------
do_prepend <- "--prepend" %in% commandArgs(trailingOnly = TRUE)

# ---------------------------------------------------------------------------
# 3. Build each dust jacket
# ---------------------------------------------------------------------------
# Output directory for the jacket PDFs
jacket_dir <- "docs/dustjackets"
if (!dir.exists(jacket_dir)) dir.create(jacket_dir, recursive = TRUE)

# Verify template exists
template_path <- "_templates/dustjacket.tex"
if (!file.exists(template_path)) {

  stop("Template not found: ", template_path,
       "\nPlace dustjacket.tex in _templates/")
}

for (j in jackets) {

  cat(sprintf("\n--- Building: %s ---\n", j$name))

  # Escape LaTeX special characters in user strings
  escape_tex <- function(x) gsub("&", "\\\\&", x)

  # Build the standalone LaTeX document
  tex_content <- sprintf(
'\\documentclass[11pt,letterpaper]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{graphicx}
\\usepackage{rotating}
\\pagestyle{empty}

%% Define jacket parameters
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

  # Write to a temp .tex file in the project root (so image paths resolve)
  tex_file <- file.path(j$name, paste0(j$name, ".tex"))
  # Actually, compile from project root so relative paths work
  tex_file <- paste0(j$name, ".tex")
  writeLines(tex_content, tex_file)

  # Compile
  compiled <- tryCatch(
    tinytex::latexmk(tex_file, engine = "pdflatex", clean = TRUE),
    error = function(e) {
      warning(sprintf("Failed to compile %s: %s", j$name, e$message))
      return(NULL)
    }
  )

  if (is.null(compiled)) next

  # Move to output directory
  out_pdf <- file.path(jacket_dir, paste0(j$name, ".pdf"))
  file.copy(compiled, out_pdf, overwrite = TRUE)

  # Clean up temp files from project root
  file.remove(tex_file)
  aux_files <- list.files(".", pattern = paste0("^", j$name, "\\.(aux|log|out)$"))
  if (length(aux_files) > 0) file.remove(aux_files)
  if (file.exists(compiled) && compiled != out_pdf) file.remove(compiled)

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

    cat(sprintf("  Done. %s now has dust jacket.\n", j$target_pdf))

  } else if (do_prepend && !file.exists(j$target_pdf)) {
    cat(sprintf("  Target PDF not found: %s (skipping prepend)\n",
                j$target_pdf))
  }
}

cat("\n=== All dust jackets built ===\n")
cat(sprintf("Output directory: %s/\n", jacket_dir))

if (!do_prepend) {
  cat("\nTo prepend jackets to compiled PDFs, re-run with:\n")
  cat("  Rscript dustjackets.R --prepend\n")
}
