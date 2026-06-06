## SOP Compliance Audit Script
## Usage: source this file from the project root after applying patches.
## Requires: stringr, dplyr, readr, fs, knitr (all tidyverse-adjacent)

library(stringr)
library(dplyr)
library(fs)

# ---------------------------------------------------------------------------
# 1. Collect all .qmd SOP files
# ---------------------------------------------------------------------------
qmd_files <- dir_ls(c("field_sops", "lab_sops"), glob = "*.qmd")

# ---------------------------------------------------------------------------
# 2. Check each file for the two required elements
# ---------------------------------------------------------------------------
check_sop <- function(path) {
  txt <- readLines(path, warn = FALSE) |> paste(collapse = "\n")
  
  has_refs_div    <- str_detect(txt, "\\{#refs\\}")
  has_definitions <- str_detect(txt, "^## Definitions", negate = FALSE) |>
    (\(x) any(str_detect(
      str_split(txt, "\n")[[1]], "^## Definitions")))()
  plain_refs       <- str_detect(txt, "^## References\n\n[-*]|^## References\n\n[A-Z]")
  
  tibble(
    file            = path_file(path),
    has_refs_div    = has_refs_div,
    has_definitions = has_definitions,
    plain_refs_list = plain_refs,
    compliant       = has_refs_div & has_definitions & !plain_refs
  )
}

audit <- purrr::map_dfr(qmd_files, check_sop)

# ---------------------------------------------------------------------------
# 3. Print non-compliant SOPs
# ---------------------------------------------------------------------------
cat("\n=== NON-COMPLIANT SOPs ===\n\n")
audit |>
  filter(!compliant) |>
  select(file, has_refs_div, has_definitions, plain_refs_list) |>
  print(n = Inf)

cat("\n=== COMPLIANCE SUMMARY ===\n\n")
cat(sprintf("Total SOPs checked: %d\n",  nrow(audit)))
cat(sprintf("Compliant:          %d\n",  sum(audit$compliant)))
cat(sprintf("Non-compliant:      %d\n",  sum(!audit$compliant)))