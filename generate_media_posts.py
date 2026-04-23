#!/usr/bin/env python3
"""
generate_media_posts.py
Reads media_manifest.json (with en_text/ru_text filled in) and generates
Hugo MD files under website_v4/newsletter/{en,hy,ru}/YYYY/YYYY-MM-DD-media/

Also downloads YouTube thumbnails as plots/header.jpg for each post.
Skips any entry missing en_text or ru_text (prints a warning).

Usage:
    python3 generate_media_posts.py [--dry-run]
"""

import json
import re
import sys
import urllib.request
from pathlib import Path

MANIFEST      = Path.home() / "R/newsletter/media_manifest.json"
WEBSITE_ROOT  = Path.home() / "docs/partnership/website_v4/newsletter"
DRY_RUN       = "--dry-run" in sys.argv

# ── YouTube helpers ────────────────────────────────────────────────────────────

def yt_embed(vid_id: str) -> str:
    """Hugo YouTube shortcode."""
    return f"{{{{< youtube {vid_id} >}}}}"

def thumbnail_url(vid_id: str) -> str:
    return f"https://img.youtube.com/vi/{vid_id}/maxresdefault.jpg"

def download_thumbnail(vid_id: str, dest: Path):
    """Download maxresdefault; fall back to hqdefault if 404."""
    for quality in ("maxresdefault", "hqdefault"):
        url = f"https://img.youtube.com/vi/{vid_id}/{quality}.jpg"
        try:
            urllib.request.urlretrieve(url, dest)
            # YouTube returns a 120x90 placeholder for missing maxresdefault
            if dest.stat().st_size > 5000:
                return
        except Exception:
            pass
    print(f"  WARNING: could not download thumbnail for {vid_id}")

# ── Text helpers ───────────────────────────────────────────────────────────────

def strip_emoji(s: str) -> str:
    """Remove leading emoji from titles like '📺 Title' → 'Title'."""
    return re.sub(r'^[\U0001F000-\U0001FFFF\U00002700-\U000027BF\s]+', '', s).strip()

def extract_title_from_text(text: str, youtube_ids: list) -> str:
    """
    Try to pull a title from the first YouTube markdown link in text:
    [📺 Some title](https://youtu.be/ID) → 'Some title'
    Falls back to empty string.
    """
    m = re.search(r'\[([^\]]+)\]\(https?://(?:youtu\.be|youtube\.com)[^\)]+\)', text)
    if m:
        return strip_emoji(m.group(1))
    return ""

def hy_title_from_text(text: str, youtube_ids: list) -> str:
    title = extract_title_from_text(text, youtube_ids)
    return title if title else "ԶԼՄ հաղորդագրություններ"

def build_body(youtube_ids: list, text: str, lang: str) -> str:
    """
    Build the markdown body for each language.
    - For hy: use hy_text directly, replacing inline YouTube links with shortcodes
    - For en/ru: use en_text/ru_text directly, inserting shortcodes after each video section
    The en/ru texts from the manifest are free-form — the user writes them however they like.
    We just ensure each YouTube ID has a shortcode somewhere.
    """
    body = text.strip()

    # Replace inline YouTube markdown links with shortcodes
    # Pattern: [![...](ytimg)](yt_url) or [text](yt_url)
    def replace_yt_link(m):
        full = m.group(0)
        vid_match = re.search(
            r'(?:youtu\.be/|youtube\.com/(?:watch\?v=|embed/))([A-Za-z0-9_-]{11})',
            full
        )
        if vid_match:
            return yt_embed(vid_match.group(1))
        return full

    # Replace markdown image links with youtube thumbnails: [![](img)](url)
    body = re.sub(r'\[!\[[^\]]*\]\([^\)]+\)\]\(([^\)]+)\)', replace_yt_link, body)
    # Replace plain markdown links to YouTube: [text](yt_url)
    body = re.sub(
        r'\[[^\]]+\]\(https?://(?:youtu\.be|www\.youtube\.com)[^\)]+\)',
        replace_yt_link,
        body
    )

    # Ensure every YouTube ID that wasn't in a link still gets a shortcode
    for vid in youtube_ids:
        if vid not in body:
            body += f"\n\n{yt_embed(vid)}"

    return body.strip()

# ── Frontmatter builder ────────────────────────────────────────────────────────

AUTHOR = {
    "en": "Aghasi Tavadyan",
    "hy": "Աղասի Թավադյան",
    "ru": "Агаси Тавадян",
}

def frontmatter(title: str, date: str, lang: str) -> str:
    author = AUTHOR[lang]
    return f"""---
title: "{title}"
date: {date}
draft: false
post_type: media
author: "{author}"
categories:
  - Economy
---"""

# ── Main ───────────────────────────────────────────────────────────────────────

def main():
    entries = json.loads(MANIFEST.read_text(encoding='utf-8'))
    print(f"Loaded {len(entries)} manifest entries")

    created = 0
    skipped = 0

    for e in entries:
        date       = e["date"]
        yt_ids     = e["youtube_ids"]
        hy_text    = e.get("hy_text", "").strip()
        en_text    = e.get("en_text", "").strip()
        ru_text    = e.get("ru_text", "").strip()

        # Skip if translations not filled in
        if not en_text or not ru_text:
            print(f"  SKIP {date}: en_text or ru_text empty")
            skipped += 1
            continue

        year = date[:4]
        dir_slug = f"{date}-media"

        # Derive titles
        hy_title = e.get("hy_title", "") or hy_title_from_text(hy_text, yt_ids)
        en_title = e.get("en_title", "") or extract_title_from_text(en_text, yt_ids)
        ru_title = e.get("ru_title", "") or extract_title_from_text(ru_text, yt_ids)

        if not hy_title: hy_title = "ԶԼՄ հաղորդագրություններ"
        if not en_title: en_title = "Media Appearances"
        if not ru_title: ru_title = "Медиавыступления"

        langs_data = {
            "en": (en_title, en_text),
            "hy": (hy_title, hy_text),
            "ru": (ru_title, ru_text),
        }

        for lang, (title, text) in langs_data.items():
            post_dir  = WEBSITE_ROOT / lang / year / dir_slug
            plots_dir = post_dir / "plots"
            index_md  = post_dir / "index.md"

            if DRY_RUN:
                print(f"  [DRY] would create {index_md}")
                continue

            plots_dir.mkdir(parents=True, exist_ok=True)

            # Download thumbnail (only once — copy from en to others)
            thumb = plots_dir / "header.jpg"
            if not thumb.exists() and yt_ids:
                download_thumbnail(yt_ids[0], thumb)

            body   = build_body(yt_ids, text, lang)
            fm     = frontmatter(title, date, lang)
            content = fm + "\n\n" + body + "\n"

            index_md.write_text(content, encoding='utf-8')

        if not DRY_RUN:
            print(f"  OK  {date}  ({len(yt_ids)} video{'s' if len(yt_ids)>1 else ''})  {hy_title[:50]}")
            created += 1

    print(f"\nCreated: {created}  Skipped (no translation): {skipped}")
    if skipped:
        print("Fill in en_text and ru_text in media_manifest.json, then re-run.")

if __name__ == "__main__":
    main()
