#!/usr/bin/env python3
"""
clean_hy_md.py — Clean up HY source markdown files before translation.

Actions (in order):
  1. Strip <!-- IMAGE_URL: ... --> comment lines
  2. Strip *** bold-italic markers (*** at start/end of citation paragraphs and URLs)
  3. Remove old inline self-citation blocks (lines starting with ***Թavadyan / ***Tavadyan
     or the surrounding triple-asterisk paragraph)
  4. Remove "## English Summary" / "# English Summary" section to end of file
  5. Replace collaboration/HAMAG block with {{< ad-banner >}} Hugo shortcode

The HAMAG block signature (any combination):
  - Contains "Algorithmic trading"  OR
  - Contains "Let's Put Your Data to Work!"
  Bounded by --- above and --- below.

Usage:
    python3 clean_hy_md.py             # all files, write in-place
    python3 clean_hy_md.py --dry-run   # preview diffs only
    python3 clean_hy_md.py --file hy/2024/2024-10-07-wages/index.md
"""

import re
import sys
import argparse
import difflib
from pathlib import Path

HY_DIR = Path.home() / "docs/partnership/website_v4/newsletter/hy"

# ── Helpers ────────────────────────────────────────────────────────────────────

def strip_image_url_comments(text: str) -> str:
    """Remove <!-- IMAGE_URL: ... --> lines."""
    return re.sub(r'<!--\s*IMAGE_URL:.*?-->\n?', '', text)


def strip_triple_asterisk_markers(text: str) -> str:
    """
    Fix *** artifacts:
      - URL ending in *** → remove the ***
      - Line starting with *** → remove leading ***
      - Line ending with *** → remove trailing ***
    """
    # URLs ending in *** (possibly with escaped underscores before)
    text = re.sub(r'\*{3}\s*$', '', text, flags=re.MULTILINE)
    # Lines starting with *** (old bold-italic citation blocks)
    text = re.sub(r'^\*{3}', '', text, flags=re.MULTILINE)
    return text


def remove_english_summary(text: str) -> str:
    """
    Remove everything from '## English Summary' or '# English Summary'
    to end of file (these are now replaced by Gemini translations).
    """
    pattern = re.compile(
        r'\n{0,2}#{1,2} English Summary\b.*',
        re.DOTALL | re.IGNORECASE
    )
    return pattern.sub('', text)


def replace_hamag_block(text: str) -> str:
    """
    Find the collaboration/HAMAG advertising block and replace with
    {{< ad-banner >}} shortcode.

    The block is identified by containing any of the anchor phrases, and is
    bounded by --- horizontal rules above and below.
    """
    ANCHORS = [
        'Algorithmic trading',
        "Let's Put Your Data to Work!",
        'ԴԻՄԵՔ ՄԵԶ',            # "CONTACT US" in Armenian
        'ԹԻMHM',                 # TEAM (Armenian)
    ]

    # Split on --- boundaries, then find and replace the section(s) containing anchors
    # Strategy: work line by line to locate the --- ... --- block containing an anchor

    lines = text.split('\n')
    # Find all --- positions
    hr_positions = [i for i, ln in enumerate(lines) if ln.strip() == '---']

    # For each --- pair, check if the block between contains an anchor
    replaced_ranges = []
    for j, start_hr in enumerate(hr_positions):
        for end_hr in hr_positions[j+1:]:
            if end_hr - start_hr > 40:  # block too large, skip
                break
            block = '\n'.join(lines[start_hr:end_hr+1])
            if any(anchor in block for anchor in ANCHORS):
                replaced_ranges.append((start_hr, end_hr))
                break  # only take the first matching end

    if not replaced_ranges:
        return text

    # Process in reverse order so indices stay valid
    for start, end in reversed(replaced_ranges):
        lines[start:end+1] = ['', '{{< ad-banner >}}', '']

    return '\n'.join(lines)


def clean_trailing_whitespace(text: str) -> str:
    """Remove trailing spaces on each line, collapse 3+ blank lines to 2."""
    lines = [ln.rstrip() for ln in text.split('\n')]
    # Collapse consecutive blank lines
    out, blank_count = [], 0
    for ln in lines:
        if ln == '':
            blank_count += 1
            if blank_count <= 2:
                out.append(ln)
        else:
            blank_count = 0
            out.append(ln)
    # Strip trailing blank lines at EOF, ensure single newline
    while out and out[-1] == '':
        out.pop()
    return '\n'.join(out) + '\n'


def clean_file(md_path: Path, dry_run: bool) -> bool:
    """Apply all cleanups to a single file. Returns True if changed."""
    original = md_path.read_text(encoding='utf-8')
    text = original

    text = strip_image_url_comments(text)
    text = strip_triple_asterisk_markers(text)
    text = remove_english_summary(text)
    text = replace_hamag_block(text)
    text = clean_trailing_whitespace(text)

    if text == original:
        return False

    if dry_run:
        diff = difflib.unified_diff(
            original.splitlines(keepends=True),
            text.splitlines(keepends=True),
            fromfile=str(md_path),
            tofile=str(md_path) + ' (cleaned)',
            n=2,
        )
        sys.stdout.writelines(diff)
    else:
        md_path.write_text(text, encoding='utf-8')

    return True


# ── Main ───────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--dry-run', action='store_true',
                        help='Print diffs without writing')
    parser.add_argument('--file', help='Process a single file')
    args = parser.parse_args()

    if args.file:
        files = [Path(args.file)]
    else:
        files = sorted(HY_DIR.rglob('index.md'))
        # Only actual newsletter files (not section _index.md)
        files = [f for f in files if f.parent.name.startswith('20')]

    changed = 0
    for md_path in files:
        rel = md_path.relative_to(HY_DIR)
        was_changed = clean_file(md_path, args.dry_run)
        if was_changed:
            changed += 1
            if not args.dry_run:
                print(f'  cleaned: {rel}')

    action = 'would change' if args.dry_run else 'changed'
    print(f'\n{action}: {changed} / {len(files)} files')


if __name__ == '__main__':
    main()
