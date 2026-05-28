# Biogeochemistry Lab -- Standard Operating Procedures

[![Build and Deploy SOPs](https://github.com/marclos/SOPs/actions/workflows/build-deploy-sops.yml/badge.svg)](https://github.com/marclos/SOPs/actions/workflows/build-deploy-sops.yml)

Standard Operating Procedures for the Environmental Analysis (EA) Program at Pomona College. The SOPs cover field sampling, laboratory analysis, instrumentation, data management, and safety for research in biogeochemistry, water quality, soil science, and molecular ecology.

**Live site:** <https://marclos.github.io/SOPs/>

**Combined PDF:** [EA-Program-SOPs-Complete.pdf](https://marclos.github.io/SOPs/EA-Program-SOPs-Complete.pdf)

## What is in this repository

Each SOP is a self-contained [Quarto](https://quarto.org/) document (`.qmd`) that renders to both a searchable HTML page and a downloadable PDF. The site is built and deployed automatically to GitHub Pages on every push to `master`.

### SOP numbering

SOPs are split into **Field** (F-series) and **Lab** (L-series) procedures:

| Range   | Category                          |
|:--------|:----------------------------------|
| 00--09  | General safety and administration |
| 10--24  | Equipment                         |
| 25--39  | Instrumentation                   |
| 40--54  | Water and aquatic methods         |
| 55--69  | Soil methods                      |
| 70--79  | Air and atmospheric methods       |
| 80--94  | Microbes, plants, and animals     |
| 95--99  | Molecular biology and DNA         |

### Repository structure

```
SOPs/
├── _quarto.yml              # Site configuration (formats, theme, sidebar)
├── _templates/               # LaTeX header for PDF output
├── index.qmd                 # SOP index page
├── merge_sop_pdfs.R          # Script to combine all PDFs into one document
├── references.bib            # Shared bibliography
├── field_sops/               # Field SOP .qmd files (F-series)
├── lab_sops/                 # Lab SOP .qmd files (L-series)
├── sops/                     # General SOPs (orientation, etc.)
├── images/                   # Shared images
├── .github/workflows/        # GitHub Actions CI/CD
└── docs/                     # Rendered output (generated, not committed)
```

## Getting started

### Prerequisites

- [R](https://cran.r-project.org/) (4.4+)
- [Quarto](https://quarto.org/docs/get-started/) (1.4+)
- [TinyTeX](https://yihui.org/tinytex/) or a full LaTeX distribution (for PDF output)
- R packages: `knitr`, `rmarkdown`, `qpdf`

### Clone Using Rstudio Server (Recommended)

Use SOP-L801 Github and R to clone on Pomona's Server. 

### Build the site locally (for local computer, not recommended)

```bash
# Clone the repository
git clone https://github.com/marclos/SOPs.git
cd SOPs

# Install R dependencies (one time)
Rscript -e 'install.packages(c("knitr", "rmarkdown", "qpdf"))'

# Render HTML site and individual PDFs
quarto render

# Preview with live reload
quarto preview

# Merge all PDFs into a single document (after rendering)
Rscript merge_sop_pdfs.R
```

The rendered site appears in `docs/`. Individual PDFs land in `docs/pdfs/`, and the combined document is `docs/EA-Program-SOPs-Complete.pdf`.

## How to contribute

Contributions from EA students, staff, and collaborators are welcome.

### Edit an existing SOP

1. Pull the latest changes (`git pull`)
2. Edit the `.qmd` file in `field_sops/` or `lab_sops/`
3. Preview locally with `quarto preview`
4. Commit with a clear message (e.g., "L58: added centrifuge speed to step 5")
5. Push to `master` (or open a Pull Request for review)

### Create a new SOP

1. Copy the template: `cp lab_sops/SOP-TEMPLATE.qmd lab_sops/SOP-LXX-short-name.qmd`
2. Fill in the YAML header (title, SOP number, author, revision history)
3. Write the procedure following the section structure in the template
4. Add a row to the index table in `index.qmd`
5. Commit and push

### Report a problem

[Open an issue](https://github.com/marclos/SOPs/issues/new) describing what needs to change and which SOP is affected.

## CI/CD

The GitHub Actions workflow (`.github/workflows/build-deploy-sops.yml`) runs on every push to `master`:

1. Installs R, TinyTeX, Quarto, and required R/LaTeX packages (cached)
2. Renders all `.qmd` files to HTML and PDF
3. Collects individual PDFs into `docs/pdfs/`
4. Merges all PDFs into a single document with a table of contents
5. Deploys the site to GitHub Pages

No tokens or secrets are needed. The only setup is enabling GitHub Pages with "Source: GitHub Actions" under **Settings > Pages**.

## License

Content is intended for use by the EA Program at Pomona College. Please contact [Marc Los Huertos](mailto:marc.loshuertos@pomona.edu) before adapting these materials for other programs.

## Acknowledgments

These SOPs have been developed over many years by EA Program students, staff, and faculty. Individual contributors are listed in each SOP's author and acknowledgments sections. The site infrastructure was built with [Quarto](https://quarto.org/) and is deployed via [GitHub Pages](https://pages.github.com/).
