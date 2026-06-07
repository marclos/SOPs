#!/usr/bin/env Rscript
# =============================================================================
# check_bib.R  --  Diagnose BibTeX parse errors in references.bib
# Run from the repo root:  Rscript check_bib.R
# =============================================================================

bib_path <- "references.bib"
bib      <- readLines(bib_path, warn = FALSE)
cat("Total lines read:", length(bib), "\n\n")

# ---- 1. Find every @type{ line and extract the key -----------------------
entry_lines <- grep("^@[A-Za-z]+\\{", bib)
cat("Total entries (@ lines):", length(entry_lines), "\n")

keys <- sub("^@[A-Za-z]+\\{([^,]+),.*", "\\1", bib[entry_lines])

# ---- 2. Detect duplicate keys --------------------------------------------
dupe_keys <- keys[duplicated(keys)]
if (length(dupe_keys) == 0L) {
  cat("OK: no duplicate keys found.\n\n")
} else {
  cat("\nWARNING: duplicate keys detected -- DELETE the later copies:\n")
  for (k in unique(dupe_keys)) {
    hits <- entry_lines[keys == k]
    cat(sprintf("  key '%s'  appears at lines: %s\n",
                k, paste(hits, collapse = ", ")))
  }
  cat("\n")
}

# ---- 3. Check the last non-blank line ------------------------------------
last_nonblank <- tail(bib[nchar(trimws(bib)) > 0], 1)
cat("Last non-blank line: [", last_nonblank, "]\n")
if (trimws(last_nonblank) == "}") {
  cat("OK: file ends with a closing brace.\n\n")
} else {
  cat("WARNING: file does NOT end with a closing brace -- likely truncated!\n\n")
}

# ---- 4. Brace-balance scan -----------------------------------------------
opens   <- cumsum(nchar(gsub("[^{]", "", bib)))
closes  <- cumsum(nchar(gsub("[^}]", "", bib)))
balance <- opens - closes

cat("Brace balance at end of file:", tail(balance, 1), "\n")
if (tail(balance, 1) != 0L) {
  cat("WARNING: unmatched braces detected.\n")
  for (i in rev(entry_lines)) {
    if (balance[i] > 0) {
      cat("Suspected broken entry starts at line", i, ":\n")
      cat("  ", bib[i], "\n")
      break
    }
  }
} else {
  cat("OK: brace balance is zero.\n")
}

# ---- 5. Summary of all entries -------------------------------------------
cat("\n--- Entry summary ---\n")
for (j in seq_along(entry_lines)) {
  flag <- if (keys[j] %in% dupe_keys) " <-- DUPLICATE" else ""
  cat(sprintf("Line %5d  %-40s%s\n", entry_lines[j], keys[j], flag))
}