#!/usr/bin/env Rscript
# =============================================================================
# check_bib.R  --  Diagnose truncated / unbalanced BibTeX entries
# Run from the repo root:  Rscript check_bib.R
# =============================================================================

bib_path <- "references.bib"
bib <- readLines(bib_path, warn = FALSE)

cat("Total lines read:", length(bib), "\n")

# ---- 1. Count @type{ opener lines ----------------------------------------
entry_starts <- grep("^@[A-Za-z]+\\{", bib)
cat("Total entries (@ lines):", length(entry_starts), "\n\n")

# ---- 2. Check last non-blank line ----------------------------------------
last_nonblank <- tail(bib[nchar(trimws(bib)) > 0], 1)
cat("Last non-blank line: [", last_nonblank, "]\n")
if (trimws(last_nonblank) == "}") {
  cat("OK: file ends with a closing brace.\n\n")
} else {
  cat("WARNING: file does NOT end with a closing brace -- likely truncated!\n\n")
}

# ---- 3. Brace-balance scan: find entries with unmatched braces -----------
opens  <- cumsum(nchar(gsub("[^{]", "", bib)))
closes <- cumsum(nchar(gsub("[^}]", "", bib)))
balance <- opens - closes

# After each entry-closing `}` the running balance should return to 0.
# Find lines where we are inside an entry (balance > 0) and then
# identify the start of the last open entry if the file ends un-closed.
cat("Brace balance at end of file:", tail(balance, 1), "\n")
if (tail(balance, 1) != 0) {
  cat("WARNING: unmatched braces detected.\n")
  # Find the last @ line whose brace balance never returns to 0 afterward
  for (i in rev(entry_starts)) {
    if (balance[i] > 0 && (i == tail(entry_starts, 1) || balance[i + 1] > 0)) {
      cat("Suspected broken entry starts at line", i, ":\n")
      cat("  ", bib[i], "\n")
      break
    }
  }
} else {
  cat("OK: brace balance is zero -- no truncated entries found.\n")
}

# ---- 4. Report entry starting at each @ for a quick visual check --------
cat("\n--- Entry summary (first 80 chars of each @ line) ---\n")
for (i in entry_starts) {
  cat(sprintf("Line %5d: %s\n", i, substr(bib[i], 1, 80)))
}