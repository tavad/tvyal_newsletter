#!/usr/bin/env python3
"""
patch_text_links.py — Add hyperlinks to text in HY markdown files using
the original HTML sources as the link source of truth.

For each newsletter:
  1. Parse the original HTML, collect all <a href="url">text</a> (non-image)
  2. Filter to substantive links (skip boilerplate footer links)
  3. For each link text, search the markdown file
  4. If text found exactly once and not already linked → wrap as [text](url)

Target: website_v4/newsletter/hy/ (the live md files, not newsletter_clean)

Usage:
    python3 patch_text_links.py              # all issues
    python3 patch_text_links.py --dry-run    # show what would be patched
    python3 patch_text_links.py --issue 2024/2024_10_07_wages
"""

import re
import sys
import argparse
import logging
from pathlib import Path
from bs4 import BeautifulSoup

NEWSLETTER_DIR = Path.home() / 'R' / 'newsletter'
WEBSITE_HY     = Path.home() / 'docs/partnership/website_v4/newsletter/hy'
LOG_FILE       = NEWSLETTER_DIR / 'patch_text_links.log'

SKIP_FOLDERS   = {'future_ideas', 'copper_story', '2024_GDP', '2023_09_04_reexports_test'}

# Boilerplate link texts that appear in every newsletter footer — skip them
SKIP_TEXTS = {
    'tvyal.com', 'tavadyan.com',
    'Բաժanordagrvek', 'Subscribe here', 'subscribe here',
    'Was this email forwarded to you? Subscribe here.',
    'databases', 'online dashboards',
    'տvyal.com',
}

# Boilerplate by URL pattern — skip these hrefs
SKIP_URL_PATTERNS = [
    r'tvyal\.com/?$',                          # bare homepage
    r'tavadyan\.com/?$',                       # author homepage
    r'tvyal\.com/subscribe',
    # NOTE: self-link (same issue) is handled per-issue below in is_boilerplate_url
]

MIN_TEXT_LEN = 5   # ignore link texts shorter than this


def setup_logging():
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s %(levelname)s %(message)s',
        handlers=[
            logging.FileHandler(LOG_FILE, encoding='utf-8'),
            logging.StreamHandler(sys.stdout),
        ]
    )


log = logging.getLogger(__name__)


def find_html(issue_path: Path) -> Path | None:
    htmls = [f for f in issue_path.iterdir()
             if f.suffix.lower() == '.html' and f.is_file()]
    if not htmls:
        return None
    tn = [h for h in htmls if h.name.startswith('TN_')]
    return sorted(tn)[0] if tn else sorted(htmls)[0]


def folder_to_slug(folder_name: str) -> str:
    p = folder_name.split('_')
    try:
        y = p[0]
        m = str(min(int(p[1]), 12)).zfill(2)
        d = str(max(int(p[2]), 1)).zfill(2)
        return f'{y}-{m}-{d}-{"-".join(p[3:]).lower()}'
    except Exception:
        return folder_name.replace('_', '-').lower()


def get_year(folder_name: str) -> str:
    return folder_name.split('_')[0]


def is_boilerplate_url(href: str, issue_folder_name: str) -> bool:
    """Return True if this URL should be skipped."""
    for pat in SKIP_URL_PATTERNS:
        if re.search(pat, href):
            return True
    # Also skip if the URL matches the current issue (self-reference)
    parts = issue_folder_name.split('_')
    if len(parts) >= 3:
        date_str = f"{parts[0]}_{parts[1]}_{parts[2]}"
        if date_str in href:
            return True
    return False


def get_text_links(html_path: Path, issue_folder_name: str) -> list[tuple[str, str]]:
    """
    Returns list of (normalized_text, href) for substantive text links.
    Skips: image links, boilerplate, short texts, URL-as-text.
    """
    raw = html_path.read_text(encoding='utf-8', errors='replace')
    soup = BeautifulSoup(raw, 'html.parser')

    # Remove Plotly divs (same as patch_image_links does for image ordering)
    for div in soup.find_all('div', class_=lambda c: c and 'plotly' in c.split()):
        div.decompose()

    seen_pairs: set[tuple[str, str]] = set()
    links: list[tuple[str, str]] = []

    for a in soup.find_all('a', href=True):
        href = a.get('href', '').strip()

        # Only http links
        if not href.startswith('http'):
            continue

        # Skip if contains an image
        if a.find('img'):
            continue

        if is_boilerplate_url(href, issue_folder_name):
            continue

        text = ' '.join(a.get_text().split())  # normalize whitespace

        # Skip empty, too short, or text that IS the URL
        if not text or len(text) < MIN_TEXT_LEN:
            continue
        if text == href or text.startswith('http'):
            continue

        # Skip boilerplate texts
        if any(skip.lower() in text.lower() for skip in SKIP_TEXTS):
            continue

        # Deduplicate: same (text, href) pair seen already → skip
        # (same href with different text IS allowed — e.g. inline cite + blockquote item)
        if (text, href) in seen_pairs:
            continue
        seen_pairs.add((text, href))

        links.append((text, href))

    return links


def normalize_md_inline(content: str) -> str:
    """
    Return a version of the markdown with multi-line list items joined.
    List items in Armenian markdown often wrap across lines like:
        - 💸🔚🏦
          Long title text
          continued here
    The HTML source has these as a single link text.
    We build a flat string for matching purposes only.
    """
    # Join continuation lines (lines indented under a list item) with a space
    joined = re.sub(r'\n[ \t]{2,}', ' ', content)
    # Also collapse blank-line-separated inline text spans (rare but safe)
    return joined


def patch_markdown(md_path: Path, links: list[tuple[str, str]], dry_run: bool) -> int:
    """
    For each (text, href), find text in md and wrap as [text](href).

    Strategy:
      1. Try exact single-line match first (fast path).
      2. If not found, search in a whitespace-normalized view of the file,
         then reconstruct the original span to replace (handles multi-line
         list items where emoji and title are on separate indented lines).

    Only patches if text appears exactly once and is not already linked.
    Returns count of patches made.
    """
    content  = md_path.read_text(encoding='utf-8')
    original = content
    patches  = 0

    for text, href in links:
        escaped = re.escape(text)

        # ── Fast path: text is on a single line ──────────────────────────
        pattern = r'(?<!\[)(' + escaped + r')(?!\])'
        matches = list(re.finditer(pattern, content))

        if len(matches) == 1:
            m = matches[0]
            before = content[max(0, m.start()-1):m.start()]
            after  = content[m.end():m.end()+1]
            if before == '[' or after == ']':
                log.debug(f"    SKIP (already linked): {text!r}")
                continue
            # Skip if the match is inside a markdown link URL — i.e. inside (...) after ]
            # This prevents turning `[x](https://foo.com/text)` into a double-nested link
            context_before = content[max(0, m.start()-200):m.start()]
            if re.search(r'\]\([^)]*$', context_before):
                log.debug(f"    SKIP (inside link URL): {text!r}")
                continue
            replacement = f'[{text}]({href})'
            content = content[:m.start()] + replacement + content[m.end():]
            log.info(f"    PATCH (inline): {text!r} -> {href}")
            patches += 1
            continue

        if len(matches) > 1:
            log.debug(f"    SKIP (ambiguous, {len(matches)}x): {text!r}")
            continue

        # ── Slow path: try matching across continuation lines ─────────────
        # Build a regex that allows arbitrary whitespace (including newlines
        # with leading spaces) between each word/token of the link text.
        # Split on whitespace, escape each token, join with flexible ws pattern.
        tokens  = text.split()
        if len(tokens) < 2:
            continue   # single token not found above → genuinely absent
        # Allow whitespace OR blockquote markers ("> ") between tokens,
        # since Armenian newsletter blockquotes split link text across lines like:
        #   > 💎🎭🔮️
        #   > Ոսկե Պատրանք...
        ws_pat  = r'(?:\s|>\s*)+'
        ml_pattern = r'(?<!\[)(' + ws_pat.join(re.escape(t) for t in tokens) + r')(?!\])'
        ml_matches = list(re.finditer(ml_pattern, content, re.DOTALL))

        if len(ml_matches) != 1:
            if len(ml_matches) > 1:
                log.debug(f"    SKIP (multi-line ambiguous, {len(ml_matches)}x): {text!r}")
            continue

        m = ml_matches[0]
        before = content[max(0, m.start()-1):m.start()]
        after  = content[m.end():m.end()+1]
        if before == '[' or after == ']':
            log.debug(f"    SKIP (already linked multi-line): {text!r}")
            continue
        context_before = content[max(0, m.start()-200):m.start()]
        if re.search(r'\]\([^)]*$', context_before):
            log.debug(f"    SKIP (inside link URL multi-line): {text!r}")
            continue

        # Replace the multi-line span with a single-line markdown link.
        # We keep the text normalized (single spaces) in the link label.
        replacement = f'[{text}]({href})'
        content = content[:m.start()] + replacement + content[m.end():]
        log.info(f"    PATCH (multi-line): {text!r} -> {href}")
        patches += 1

    if patches > 0:
        if not dry_run:
            md_path.write_text(content, encoding='utf-8')
        else:
            import difflib
            diff = difflib.unified_diff(
                original.splitlines(keepends=True),
                content.splitlines(keepends=True),
                fromfile=str(md_path),
                tofile=str(md_path) + ' (patched)',
                n=2,
            )
            sys.stdout.writelines(diff)

    return patches


def process_issue(issue_path: Path, dry_run: bool) -> int:
    fn = issue_path.name

    html_file = find_html(issue_path)
    if not html_file:
        return 0

    links = get_text_links(html_file, fn)
    if not links:
        return 0

    year = get_year(fn)
    slug = folder_to_slug(fn)
    md_path = WEBSITE_HY / year / slug / 'index.md'

    if not md_path.exists():
        log.warning(f'  [SKIP] not found: {md_path}')
        return 0

    log.info(f'Processing: {fn} ({len(links)} candidate link(s))')
    patches = patch_markdown(md_path, links, dry_run)

    if patches == 0:
        log.info('  (no unambiguous matches)')
    else:
        log.info(f'  patched {patches} link(s)')

    return patches


def collect_issues() -> list[Path]:
    issues = []
    for yd in sorted(NEWSLETTER_DIR.iterdir()):
        if not yd.is_dir() or not yd.name.isdigit():
            continue
        for issue_dir in sorted(yd.iterdir()):
            if not issue_dir.is_dir():
                continue
            if issue_dir.name in SKIP_FOLDERS:
                continue
            issues.append(issue_dir)
    return issues


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--dry-run', action='store_true')
    parser.add_argument('--issue', help='e.g. 2024/2024_10_07_wages')
    args = parser.parse_args()

    setup_logging()
    if args.dry_run:
        log.info('DRY RUN — nothing written')

    if args.issue:
        p = NEWSLETTER_DIR / args.issue
        if not p.exists():
            sys.exit(f'Not found: {p}')
        issues = [p]
    else:
        issues = collect_issues()

    log.info(f'{len(issues)} issues to scan')

    total_patches = 0
    issues_patched = 0

    for ip in issues:
        try:
            n = process_issue(ip, args.dry_run)
            if n > 0:
                total_patches += n
                issues_patched += 1
        except Exception as e:
            log.error(f'[ERROR] {ip.name}: {e}', exc_info=True)

    log.info('')
    log.info('=' * 50)
    log.info(f'  Issues with text links patched: {issues_patched}')
    log.info(f'  Total links patched:            {total_patches}')
    if args.dry_run:
        log.info('  (dry run — nothing written)')


if __name__ == '__main__':
    main()
