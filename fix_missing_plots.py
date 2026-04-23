#!/usr/bin/env python3
"""
Fix missing plot images in newsletter issues.

For each newsletter issue, checks if plot_NN.ext files referenced in the
HY markdown are actually present. For missing ones, looks up the source
image from the corresponding R HTML file and copies it.

Image source priority:
1. HTML <img src="https://raw.githubusercontent.com/.../plots/fname.ext">
   → find fname.ext in R issue's plots/ or media/ dir
2. HTML <img src="plots/fname.ext"> (relative local path)
   → copy from R issue dir directly

Usage:
    python3 fix_missing_plots.py [--dry-run] [--issue 2024/2024-10-28-brics]
"""

import re
import sys
import shutil
import argparse
from pathlib import Path

WEBSITE_BASE = Path.home() / "docs/partnership/website_v4/newsletter"
R_BASE = Path.home() / "R/newsletter"
LANGS = ["hy", "en", "ru"]

# GitHub raw URL prefix for this repo
GITHUB_PREFIX = "https://raw.githubusercontent.com/tavad/tvyal_newsletter/refs/heads/main/"


def slug_to_r_folder(year: str, slug: str) -> Path | None:
    """Map e.g. '2024', '2024-10-28-brics' → ~/R/newsletter/2024/2024_10_28_brics."""
    # Convert slug to R underscore format
    r_name = slug.replace("-", "_")
    candidate = R_BASE / year / r_name
    if candidate.exists():
        return candidate
    # Fuzzy fallback: find R folder that starts with same date prefix
    date_prefix = "_".join(slug.split("-")[:3])  # e.g. "2024_10_28"
    year_dir = R_BASE / year
    if not year_dir.exists():
        return None
    for d in sorted(year_dir.iterdir()):
        if d.is_dir() and d.name.startswith(date_prefix):
            return d
    return None


def find_html(r_dir: Path) -> Path | None:
    """Find the main HTML file in an R issue directory."""
    htmls = [f for f in r_dir.glob("*.html") if not f.name.endswith("_files")]
    if not htmls:
        return None
    # Prefer shortest name (most likely the main one, not subfiles)
    return min(htmls, key=lambda f: len(f.name))


def parse_html_images(html_path: Path) -> list[str]:
    """Return list of image src values in document order from HTML file."""
    try:
        content = html_path.read_text(errors="replace")
    except Exception as e:
        print(f"  ERROR reading {html_path}: {e}")
        return []
    # Fast regex extraction — no bs4 needed
    srcs = re.findall(r'<img[^>]+src=["\']([^"\']+)["\']', content, re.IGNORECASE)
    return srcs


def resolve_image_src(src: str, r_dir: Path) -> Path | None:
    """Given an img src, return the local file path if resolvable."""
    if src.startswith("data:"):
        # base64 embedded — already handled by migrate script
        return None

    if src.startswith(GITHUB_PREFIX):
        # e.g. .../2024/2024_10_28_brics/plots/plot_brics_1.png
        rel = src[len(GITHUB_PREFIX):]  # e.g. 2024/2024_10_28_brics/plots/fname.ext
        parts = Path(rel).parts  # ('2024', '2024_10_28_brics', 'plots', 'fname.ext')
        if len(parts) >= 2:
            fname = parts[-1]
            subdir = parts[-2]  # 'plots' or 'media'
            candidate = r_dir / subdir / fname
            if candidate.exists():
                return candidate
            # Also try just plots/ regardless of subdir
            for sub in ["plots", "media", "."]:
                c = r_dir / sub / fname
                if c.exists():
                    return c
        return None

    if src.startswith("http"):
        # Some other URL — can't resolve locally
        return None

    # Relative path like "plots/fname.png"
    candidate = r_dir / src
    if candidate.exists():
        return candidate
    # Try just the filename
    fname = Path(src).name
    for sub in ["plots", "media", "."]:
        c = r_dir / sub / fname
        if c.exists():
            return c
    return None


def get_missing_refs(md_path: Path) -> list[tuple[int, str]]:
    """Return list of (N, filename) for plot_NN.ext refs that are missing from plots/."""
    content = md_path.read_text(errors="replace")
    plots_dir = md_path.parent / "plots"
    missing = []
    for m in re.finditer(r'!\[\]\(plots/(plot_(\d+)\.[a-zA-Z]+)\)', content):
        fname = m.group(1)   # e.g. plot_01.png
        n = int(m.group(2))  # e.g. 1
        if not (plots_dir / fname).exists():
            missing.append((n, fname))
    return missing


def fix_issue(year: str, slug: str, dry_run: bool) -> dict:
    result = {"issue": f"{year}/{slug}", "fixed": [], "skipped": [], "errors": []}

    hy_md = WEBSITE_BASE / "hy" / year / slug / "index.md"
    if not hy_md.exists():
        result["errors"].append("no HY index.md")
        return result

    missing = get_missing_refs(hy_md)
    if not missing:
        return result  # nothing to do

    r_dir = slug_to_r_folder(year, slug)
    if not r_dir:
        result["errors"].append(f"no R dir found for {year}/{slug}")
        return result

    html_path = find_html(r_dir)
    if not html_path:
        result["errors"].append(f"no HTML in {r_dir}")
        return result

    srcs = parse_html_images(html_path)
    # Filter out base64 for indexing purposes; they were already extracted
    non_base64_srcs = [s for s in srcs if not s.startswith("data:")]

    for (n, expected_fname) in missing:
        idx = n - 1  # 0-based index
        if idx >= len(non_base64_srcs):
            result["errors"].append(f"  {expected_fname}: HTML only has {len(non_base64_srcs)} non-base64 images")
            continue

        src = non_base64_srcs[idx]
        local = resolve_image_src(src, r_dir)
        if not local:
            result["errors"].append(f"  {expected_fname}: cannot resolve src {src[:80]}")
            continue

        # Determine actual extension from source file
        actual_ext = local.suffix.lower()  # e.g. .png, .jpg
        # Use expected_fname but swap extension if different
        expected_ext = "." + expected_fname.split(".")[-1].lower()
        dest_fname = expected_fname
        if actual_ext != expected_ext:
            dest_fname = expected_fname.rsplit(".", 1)[0] + actual_ext

        # Copy to all language dirs
        for lang in LANGS:
            dest_dir = WEBSITE_BASE / lang / year / slug / "plots"
            dest = dest_dir / dest_fname
            if dest.exists():
                continue
            if not dry_run:
                dest_dir.mkdir(parents=True, exist_ok=True)
                shutil.copy2(local, dest)
            result["fixed"].append(f"  {lang}/{year}/{slug}/plots/{dest_fname} ← {local.name}")

        # If dest_fname != expected_fname, we need to also update the markdown
        if dest_fname != expected_fname and not dry_run:
            for lang in LANGS:
                md = WEBSITE_BASE / lang / year / slug / "index.md"
                if md.exists():
                    txt = md.read_text(errors="replace")
                    new_txt = txt.replace(f"plots/{expected_fname}", f"plots/{dest_fname}")
                    if new_txt != txt:
                        md.write_text(new_txt)
                        result["fixed"].append(f"  updated {lang} markdown: {expected_fname} → {dest_fname}")

    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dry-run", action="store_true", help="Report only, don't copy")
    parser.add_argument("--issue", help="Process only this issue, e.g. 2024/2024-10-28-brics")
    args = parser.parse_args()

    if args.issue:
        parts = args.issue.split("/")
        if len(parts) != 2:
            sys.exit("--issue must be YYYY/YYYY-MM-DD-slug")
        issues = [(parts[0], parts[1])]
    else:
        # Discover all issues in HY
        issues = []
        for year_dir in sorted((WEBSITE_BASE / "hy").iterdir()):
            if not year_dir.is_dir():
                continue
            for issue_dir in sorted(year_dir.iterdir()):
                if issue_dir.is_dir() and (issue_dir / "index.md").exists():
                    issues.append((year_dir.name, issue_dir.name))

    total_fixed = 0
    for year, slug in issues:
        r = fix_issue(year, slug, args.dry_run)
        if r["fixed"] or r["errors"]:
            print(f"\n{'[DRY-RUN] ' if args.dry_run else ''}{r['issue']}")
            for line in r["fixed"]:
                print(f"  FIXED: {line}")
            for line in r["errors"]:
                print(f"  ERROR: {line}")
            total_fixed += len(r["fixed"])

    print(f"\n{'[DRY-RUN] ' if args.dry_run else ''}Done. {'Would fix' if args.dry_run else 'Fixed'} {total_fixed} files.")


if __name__ == "__main__":
    main()
