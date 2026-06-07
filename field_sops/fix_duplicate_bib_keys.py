#!/usr/bin/env python3
"""
fix_duplicate_bib_keys.py
=========================
Removes duplicate BibTeX entries from references.bib, keeping only
the FIRST occurrence of each key.

Usage (from repo root):
    python3 fix_duplicate_bib_keys.py references.bib

The script:
1. Parses every @type{key, ...} entry boundary
2. Records the first occurrence of each key
3. Removes any subsequent occurrences (the duplicates)
4. Writes the cleaned file back in-place
5. Reports what was removed

Safe: it reads the whole file as text, so unusual formatting,
accents, and non-ASCII characters are preserved unchanged.
"""

import re
import sys
from pathlib import Path


def find_entries(text: str):
    """
    Yield (start, end, key) for every top-level BibTeX entry in text.
    'end' is the index just after the closing brace of the entry.
    Handles nested braces correctly.
    """
    # Match the opening of an entry: @type{key,  or  @type{key\n
    entry_open = re.compile(
        r'@\s*[A-Za-z]+\s*\{\s*([^,\s\}]+)\s*[,\n]',
        re.IGNORECASE
    )

    for m in entry_open.finditer(text):
        key = m.group(1).strip()
        start = m.start()
        # Walk forward counting braces to find the matching close
        depth = 0
        i = m.start()
        while i < len(text):
            c = text[i]
            if c == '{':
                depth += 1
            elif c == '}':
                depth -= 1
                if depth == 0:
                    end = i + 1
                    yield (start, end, key)
                    break
            i += 1


def remove_duplicates(bib_path: Path):
    text = bib_path.read_text(encoding='utf-8')

    entries = list(find_entries(text))
    print(f"Total entries found: {len(entries)}")

    seen = {}       # key -> index of first entry
    to_remove = []  # list of (start, end) ranges to delete

    for start, end, key in entries:
        key_lower = key.lower()
        if key_lower in seen:
            print(f"  DUPLICATE: @{key}  (first seen at char {seen[key_lower]}, "
                  f"duplicate at char {start})")
            to_remove.append((start, end))
        else:
            seen[key_lower] = start

    if not to_remove:
        print("No duplicates found -- references.bib is clean.")
        return

    # Remove duplicates from end to start so offsets stay valid
    to_remove.sort(key=lambda x: x[0], reverse=True)
    result = list(text)

    for start, end in to_remove:
        # Also consume any leading blank lines before the entry
        ws_start = start
        while ws_start > 0 and result[ws_start - 1] in (' ', '\t', '\n', '\r'):
            ws_start -= 1
        # Consume one trailing newline after the closing brace if present
        ws_end = end
        if ws_end < len(result) and result[ws_end] == '\n':
            ws_end += 1
        del result[ws_start:ws_end]

    cleaned = ''.join(result)
    bib_path.write_text(cleaned, encoding='utf-8')
    print(f"\nRemoved {len(to_remove)} duplicate entr{'y' if len(to_remove)==1 else 'ies'}.")
    print("Cleaned file written back to:", bib_path)

    # Quick brace-balance check
    opens  = cleaned.count('{')
    closes = cleaned.count('}')
    balance = opens - closes
    print(f"\nBrace balance: {opens} open, {closes} close => {balance}")
    if balance == 0:
        print("OK: braces balanced.")
    else:
        print("WARNING: braces unbalanced -- check the file manually.")


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python3 fix_duplicate_bib_keys.py references.bib")
        sys.exit(1)
    bib = Path(sys.argv[1])
    if not bib.exists():
        print(f"File not found: {bib}")
        sys.exit(1)
    remove_duplicates(bib)
