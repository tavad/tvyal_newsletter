#!/usr/bin/env python3
"""
extract_media.py
Scans all Rmd files in ~/R/newsletter/ for ԶԼՄ (mass media) sections,
extracts YouTube IDs and Armenian text, and outputs media_manifest.json
for manual EN/RU translation before Hugo post generation.

Usage:
    python3 extract_media.py
Output:
    ~/R/newsletter/media_manifest.json
"""

import os
import re
import json
from pathlib import Path

NEWSLETTER_ROOT = Path.home() / "R" / "newsletter"
OUTPUT_FILE     = NEWSLETTER_ROOT / "media_manifest.json"

# ── Regex helpers ──────────────────────────────────────────────────────────────

# YAML frontmatter date (quoted or unquoted)
RE_DATE = re.compile(r'^date:\s*["\']?(\d{4}-\d{2}-\d{2})["\']?', re.MULTILINE)

# ԶԼՄ section header (handles numbered variants like "## 2. ԶԼՄ ...")
RE_ZLM_SECTION = re.compile(
    r'(?:^|\n)(#{1,3}\s+(?:\d+\.\s+)?ԶԼՄ[^\n]*)\n(.*?)(?=\n#{1,3} |\Z)',
    re.DOTALL
)

# YouTube IDs from all common URL forms:
#   youtu.be/ID  |  youtube.com/watch?v=ID  |  youtube.com/embed/ID
RE_YT = re.compile(
    r'(?:youtu\.be/|youtube\.com/(?:watch\?v=|embed/))([A-Za-z0-9_-]{11})'
)

# Boilerplate lines to strip from extracted text
BOILERPLATE_PATTERNS = [
    re.compile(r'Հարգանքներով.*', re.DOTALL),      # signature block onwards
    re.compile(r'\[Was this email.*', re.DOTALL),
    re.compile(r'\[Բաժանորդագրվեք\].*', re.DOTALL),
    re.compile(r'<hr>.*', re.DOTALL),
    re.compile(r'#{3,}.*', re.DOTALL),              # ###### footer dividers
    re.compile(r'Ներկայացնենք վերջին.*?\n'),        # "Here are last N weeks media"
]

# HTML tags to remove (thumbnail <a><img> blocks)
RE_HTML_TAG = re.compile(r'<[^>]+>')

# ── Text cleaner ───────────────────────────────────────────────────────────────

def clean_text(raw: str) -> str:
    """Strip boilerplate, HTML tags, thumbnail lines, and excess whitespace."""
    text = raw

    # Strip boilerplate (apply in order — first match wins)
    for pat in BOILERPLATE_PATTERNS:
        text = pat.sub('', text)

    # Remove HTML tags
    text = RE_HTML_TAG.sub('', text)

    # Remove lines that are just a ytimg.com URL or empty after stripping
    lines = []
    for line in text.splitlines():
        stripped = line.strip()
        if 'ytimg.com' in stripped:
            continue
        if stripped == '-----' or stripped == '---':
            continue
        lines.append(line)

    text = '\n'.join(lines)

    # Collapse 3+ blank lines into 2
    text = re.sub(r'\n{3,}', '\n\n', text)

    return text.strip()

# ── Date normalizer ────────────────────────────────────────────────────────────

def extract_date(content: str, filepath: Path) -> str | None:
    """Extract YYYY-MM-DD from frontmatter, fall back to directory name."""
    m = RE_DATE.search(content)
    if m:
        return m.group(1)
    # Fallback: parse from directory name like 2024_01_07_household_income
    parts = filepath.parent.name.split('_')
    if len(parts) >= 3:
        try:
            return f"{parts[0]}-{parts[1]}-{parts[2]}"
        except Exception:
            pass
    return None

# ── Main extraction ────────────────────────────────────────────────────────────

def extract_from_file(rmd_path: Path) -> dict | None:
    content = rmd_path.read_text(encoding='utf-8', errors='replace')

    m = RE_ZLM_SECTION.search(content)
    if not m:
        return None

    section_body = m.group(2)
    yt_ids = list(dict.fromkeys(RE_YT.findall(section_body)))  # dedup, preserve order
    if not yt_ids:
        return None  # No actual videos — skip

    date = extract_date(content, rmd_path)
    if not date:
        print(f"  WARNING: could not determine date for {rmd_path}")
        return None

    hy_text = clean_text(section_body)

    return {
        "date":        date,
        "source_file": str(rmd_path.relative_to(NEWSLETTER_ROOT)),
        "youtube_ids": yt_ids,
        "hy_text":     hy_text,
        "en_title":    "",   # fill before running generate_media_posts.py
        "en_text":     "",
        "ru_title":    "",
        "ru_text":     "",
    }

# ── Dedup by date ─────────────────────────────────────────────────────────────

def merge_by_date(entries: list[dict]) -> list[dict]:
    """
    If multiple Rmd files share the same date (e.g. main + new_ variant),
    merge their youtube_ids and concatenate hy_text.
    Primary file (shorter name / non-new_ prefix) goes first.
    """
    by_date: dict[str, dict] = {}
    for e in entries:
        d = e["date"]
        if d not in by_date:
            by_date[d] = e
        else:
            existing = by_date[d]
            # Merge YouTube IDs (dedup)
            merged_ids = list(dict.fromkeys(existing["youtube_ids"] + e["youtube_ids"]))
            existing["youtube_ids"] = merged_ids
            # Append text only if it adds new content
            if e["hy_text"] and e["hy_text"] not in existing["hy_text"]:
                existing["hy_text"] += "\n\n" + e["hy_text"]
            existing["source_file"] += " + " + e["source_file"]

    return sorted(by_date.values(), key=lambda x: x["date"])

# ── Run ────────────────────────────────────────────────────────────────────────

def main():
    rmd_files = sorted(NEWSLETTER_ROOT.rglob("*.Rmd"))
    print(f"Found {len(rmd_files)} Rmd files total")

    entries = []
    skipped = []
    for rmd in rmd_files:
        # Skip future_ideas and non-issue dirs
        if 'future_ideas' in str(rmd):
            continue
        result = extract_from_file(rmd)
        if result:
            entries.append(result)
        else:
            skipped.append(rmd.name)

    print(f"Extracted ԶԼՄ sections: {len(entries)}")
    print(f"Files with no ԶԼՄ or no YouTube: {len(skipped)}")

    merged = merge_by_date(entries)
    print(f"Unique dates after merge: {len(merged)}")

    # Write manifest
    OUTPUT_FILE.write_text(
        json.dumps(merged, ensure_ascii=False, indent=2),
        encoding='utf-8'
    )
    print(f"\nManifest written to: {OUTPUT_FILE}")
    print("\nNext steps:")
    print("  1. Open media_manifest.json")
    print("  2. Fill in en_title, en_text, ru_title, ru_text for each entry")
    print("  3. Run generate_media_posts.py to create Hugo MD files")

    # Print summary table
    print("\n── Summary ──────────────────────────────────────────")
    for e in merged:
        n = len(e["youtube_ids"])
        print(f"  {e['date']}  {n} video{'s' if n > 1 else ''}  {e['source_file'][:60]}")

if __name__ == "__main__":
    main()
