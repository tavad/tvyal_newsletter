#!/usr/bin/env python3
"""
Retrospective patch: add hyperlinks to markdown images where the original
HTML had <a href="..."><img .../></a>.

For each newsletter issue:
1. Read original HTML, find all <img> tags in order (same order migration used)
2. For each img that has a parent <a href="...">, note its position N and href
3. In the output index.md, replace ![](plots/plot_NN.ext) with [![](plots/plot_NN.ext)](href)

Run:
    python3 patch_image_links.py              # all issues
    python3 patch_image_links.py --dry-run    # preview only
    python3 patch_image_links.py --issue 2023/2023_11_20_petrol_price
"""

import re, sys, argparse, logging
from pathlib import Path
from bs4 import BeautifulSoup

NEWSLETTER_DIR = Path.home() / "R" / "newsletter"
OUTPUT_BASE    = Path.home() / "R" / "newsletter_clean" / "hy"
LOG_FILE       = OUTPUT_BASE / "patch_image_links.log"

SKIP_FOLDERS  = {"future_ideas","copper_story","2024_GDP","2023_09_04_reexports_test"}
NO_HTML_KNOWN = {"2023_12_04_exports_quality","2024_07_12_inflation_map","2024_09_09_GDP_components"}

def setup_logging():
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)s %(message)s",
        handlers=[
            logging.FileHandler(LOG_FILE, encoding="utf-8"),
            logging.StreamHandler(sys.stdout),
        ]
    )

log = logging.getLogger(__name__)

def find_html(issue_path):
    htmls = [f for f in issue_path.iterdir()
              if f.suffix.lower() == ".html" and f.is_file()]
    if not htmls: return None
    tn = [h for h in htmls if h.name.startswith("TN_")]
    return sorted(tn)[0] if tn else sorted(htmls)[0]

def get_linked_images(html_path):
    """
    Returns dict: {img_index (1-based): href}
    img_index matches the plot_{N:02d} numbering used during migration.
    Only includes images that had a parent <a href>.
    Skips Plotly widget divs (same as migration did).
    """
    raw = html_path.read_text(encoding="utf-8", errors="replace")
    soup = BeautifulSoup(raw, "html.parser")

    # Remove Plotly divs so image indexing matches migration exactly
    for div in soup.find_all("div", class_=lambda c: c and "plotly" in c.split()):
        div.decompose()

    linked = {}
    for i, img in enumerate(soup.find_all("img"), start=1):
        parent = img.parent
        if parent and parent.name == "a":
            href = parent.get("href", "").strip()
            if href and href.startswith("http"):
                linked[i] = href

    return linked

def folder_to_output_name(folder_name):
    p = folder_name.split("_")
    try:
        y=p[0]; m=str(min(int(p[1]),12)).zfill(2); d=str(max(int(p[2]),1)).zfill(2)
        return f"{y}-{m}-{d}-{'-'.join(p[3:]).lower()}"
    except: return folder_name.replace("_","-").lower()

def get_year(fn): return fn.split("_")[0]

def patch_markdown(md_path, linked_images, dry_run):
    """
    For each (img_index -> href), find ![](plots/plot_NN.ext) in the markdown
    and replace with [![](plots/plot_NN.ext)](href).
    Skips images that are already linked (have [! prefix).
    Returns count of patches made.
    """
    content = md_path.read_text(encoding="utf-8")
    original = content
    patches = 0

    for idx, href in linked_images.items():
        # Match plot_NN with any extension, not already inside a link
        # Negative lookbehind for [ ensures we don't double-wrap
        pattern = r'(?<!\[)(!\[\]\(plots/plot_{:02d}\.[a-z]+\))'.format(idx)
        replacement = r'[\1]({})'.format(href)
        new_content, n = re.subn(pattern, replacement, content)
        if n > 0:
            content = new_content
            patches += n
            log.info(f"    img[{idx:02d}] -> {href}")

    if patches > 0 and not dry_run:
        md_path.write_text(content, encoding="utf-8")

    return patches

def process_issue(issue_path, dry_run):
    fn = issue_path.name

    html_file = find_html(issue_path)
    if not html_file:
        return 0

    linked = get_linked_images(html_file)
    if not linked:
        return 0

    year     = get_year(fn)
    out_name = folder_to_output_name(fn)
    md_path  = OUTPUT_BASE / year / out_name / "index.md"

    if not md_path.exists():
        log.warning(f"  [SKIP] index.md not found: {md_path}")
        return 0

    log.info(f"Patching: {fn} ({len(linked)} linked image(s))")
    patches = patch_markdown(md_path, linked, dry_run)

    if patches == 0:
        log.info(f"  (already patched or images not found in md)")
    return patches

def collect_issues():
    issues = []
    for yd in sorted(NEWSLETTER_DIR.iterdir()):
        if not yd.is_dir() or not yd.name.isdigit(): continue
        for id_ in sorted(yd.iterdir()):
            if not id_.is_dir(): continue
            if id_.name in SKIP_FOLDERS | NO_HTML_KNOWN: continue
            issues.append(id_)
    return issues

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--issue")
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()

    setup_logging()
    if args.dry_run: log.info("DRY RUN — nothing written")

    if args.issue:
        p = NEWSLETTER_DIR / args.issue
        if not p.exists(): sys.exit(f"Not found: {p}")
        issues = [p]
    else:
        issues = collect_issues()

    log.info(f"{len(issues)} issues to scan")

    total_patches = 0
    issues_patched = 0

    for ip in issues:
        try:
            n = process_issue(ip, args.dry_run)
            if n > 0:
                total_patches += n
                issues_patched += 1
        except Exception as e:
            log.error(f"[ERROR] {ip.name}: {e}", exc_info=True)

    log.info("")
    log.info("=" * 50)
    log.info(f"  Issues with linked images: {issues_patched}")
    log.info(f"  Total links patched:       {total_patches}")
    if args.dry_run: log.info("  (dry run — nothing written)")

if __name__ == "__main__":
    main()
