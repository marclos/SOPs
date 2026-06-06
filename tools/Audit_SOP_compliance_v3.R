library(stringr)
library(dplyr)
library(fs)

qmd_files <- dir_ls(c("field_sops", "lab_sops"), glob = "*.qmd")

check_sop <- function(path) {
  txt  <- readLines(path, warn = FALSE)
  full <- paste(txt, collapse = "\n")
  
  has_refs_div    <- str_detect(full, "\\{#refs\\}")
  has_definitions <- any(str_detect(txt, "^## Definitions\\s*$"))
  
  refs_line <- which(str_detect(txt, "^## References\\s*$"))
  
  plain_refs_list <- FALSE
  if (length(refs_line) >= 1L) {
    start <- refs_line[1L] + 1L
    end   <- min(refs_line[1L] + 10L, length(txt))
    if (start <= end) {
      window <- txt[seq(start, end)]
      window <- window[nchar(trimws(window)) > 0]
      if (length(window) > 0) {
        has_div   <- any(str_detect(window, "\\{#refs\\}"))
        has_plain <- any(str_detect(window,
                                    "^[-*]|^[A-Z]|^https?://|^<https?://|^@"))
        plain_refs_list <- has_plain & !has_div
      }
    }
  }
  
  tibble(
    file            = path_file(path),
    has_refs_div    = has_refs_div,
    has_definitions = has_definitions,
    plain_refs_list = plain_refs_list,
    compliant       = has_refs_div & has_definitions & !plain_refs_list
  )
}

audit <- purrr::map_dfr(qmd_files, check_sop)

# ── Non-compliant detail ─────────────────────────────────────────────────────
cat("\n=== NON-COMPLIANT SOPs ===\n\n")
non_compliant <- audit |> filter(!compliant) |> arrange(file)

if (nrow(non_compliant) == 0L) {
  cat("All SOPs are compliant.\n")
} else {
  non_compliant |>
    rowwise() |>
    mutate(
      issues = c(
        if (!has_refs_div)    "no {#refs}"     else NULL,
        if (!has_definitions) "no Definitions" else NULL,
        if (plain_refs_list)  "plain refs"     else NULL
      ) |> paste(collapse = " | ")
    ) |>
    ungroup() |>
    select(file, issues) |>
    print(n = Inf)
}

# ── Summary ──────────────────────────────────────────────────────────────────
cat("\n=== SUMMARY ===\n")
cat(sprintf("SOPs checked:             %d\n", nrow(audit)))
cat(sprintf("Fully compliant:          %d\n", sum( audit$compliant)))
cat(sprintf("Non-compliant:            %d\n", sum(!audit$compliant)))
cat(sprintf("  Missing {#refs} div:    %d\n", sum(!audit$has_refs_div)))
cat(sprintf("  Missing ## Definitions: %d\n", sum(!audit$has_definitions)))
cat(sprintf("  Plain-text refs:        %d\n", sum( audit$plain_refs_list)))