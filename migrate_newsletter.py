#!/usr/bin/env python3
"""
Newsletter Migration: ~/R/newsletter -> ~/R/newsletter_clean/hy
Converts Pandoc HTML issues to Hugo leaf bundles, year-bucketed to match
old URL structure: newsletter/YYYY/YYYY-MM-DD-slug/

Usage:
    python3 migrate_newsletter.py
    python3 migrate_newsletter.py --issue 2023/2023_11_20_petrol_price
    python3 migrate_newsletter.py --dry-run

Requirements: pip install beautifulsoup4 markdownify
"""

import re, sys, base64, shutil, logging, argparse
from pathlib import Path
from datetime import datetime

try:
    from bs4 import BeautifulSoup, Comment
except ImportError:
    sys.exit("ERROR: pip install beautifulsoup4")

try:
    import markdownify as md_lib
except ImportError:
    sys.exit("ERROR: pip install markdownify")

NEWSLETTER_DIR = Path.home() / "R" / "newsletter"
OUTPUT_BASE    = Path.home() / "R" / "newsletter_clean" / "hy"
LOG_FILE       = OUTPUT_BASE / "migrate.log"

SKIP_FOLDERS = {"future_ideas", "copper_story", "2024_GDP", "2023_09_04_reexports_test"}
NO_HTML_KNOWN = {"2023_12_04_exports_quality", "2024_07_12_inflation_map", "2024_09_09_GDP_components"}

IFRAME_SHORTCODE = """\
<iframe src="{{ .Get "src" }}"
        height="{{ .Get "height" | default "520px" }}"
        width="100%" frameborder="0" scrolling="no"
        style="border:none; overflow:hidden;">
</iframe>
"""

def make_plotly_html(widget_id, div_html, json_script_html):
    return (
        "<!DOCTYPE html>\n<html lang='hy'>\n<head>\n"
        "<meta charset='utf-8'>\n"
        "<meta name='viewport' content='width=device-width, initial-scale=1'>\n"
        "<style>body{margin:0;padding:4px;background:#fff}"
        ".plotly-graph-div{width:100%!important;height:auto!important}</style>\n"
        "<script src='https://cdn.plot.ly/plotly-2.27.0.min.js' charset='utf-8'></script>\n"
        "</head>\n<body>\n"
        + div_html + "\n"
        + json_script_html + "\n"
        "<script>\n"
        "(function(){\n"
        f"  var el=document.getElementById('{widget_id}');\n"
        f"  var ds=document.querySelector('script[data-for=\"{widget_id}\"]');\n"
        "  if(!el||!ds)return;\n"
        "  var spec=JSON.parse(ds.textContent);\n"
        "  var data=(spec.x&&spec.x.data)||[];\n"
        "  var layout=(spec.x&&spec.x.layout)||{};\n"
        "  var config=(spec.x&&spec.x.config)||{};\n"
        "  layout.autosize=true; delete layout.width; delete layout.height;\n"
        "  Plotly.react(el,data,layout,config);\n"
        "})();\n"
        "</script>\n</body>\n</html>"
    )

def setup_logging():
    OUTPUT_BASE.mkdir(parents=True, exist_ok=True)
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)s %(message)s",
        handlers=[
            logging.FileHandler(LOG_FILE, encoding="utf-8"),
            logging.StreamHandler(sys.stdout),
        ]
    )

log = logging.getLogger(__name__)

def parse_date(folder_name):
    parts = folder_name.split("_")
    try:
        y = int(parts[0]); m = min(int(parts[1]),12); d = max(min(int(parts[2]),31),1)
        return datetime(y,m,d).strftime("%Y-%m-%d")
    except: return "1970-01-01"

def folder_to_slug(fn):
    return "-".join(fn.split("_")[3:]).lower()

def folder_to_output_name(fn):
    p = fn.split("_")
    try:
        y=p[0]; m=str(min(int(p[1]),12)).zfill(2); d=str(max(int(p[2]),1)).zfill(2)
        return f"{y}-{m}-{d}-{'-'.join(p[3:]).lower()}"
    except: return fn.replace("_","-").lower()

def get_year(fn): return fn.split("_")[0]

def find_html(issue_path):
    htmls=[f for f in issue_path.iterdir() if f.suffix.lower()==".html" and f.is_file()]
    if not htmls: return None
    tn=[h for h in htmls if h.name.startswith("TN_")]
    return sorted(tn)[0] if tn else sorted(htmls)[0]

def extract_plotly_widgets(soup):
    widgets=[]
    for i,div in enumerate(soup.find_all("div", class_=lambda c: c and "plotly" in c.split())):
        wid=div.get("id",f"plotly-widget-{i+1}")
        div_html=str(div); script_html=""
        js=soup.find("script",{"data-for":wid})
        if js: script_html=str(js); js.decompose()
        widgets.append((wid,div_html,script_html))
        div.replace_with(soup.new_string(f"PLOTLY_PLACEHOLDER_{i+1}"))
    return widgets

def save_plotly_widgets(widgets, out_plotly_dir, dry_run):
    if not widgets: return
    if not dry_run: out_plotly_dir.mkdir(parents=True, exist_ok=True)
    for i,(wid,div_html,js_html) in enumerate(widgets):
        fname=f"plotly_{i+1:02d}.html"
        if not dry_run:
            (out_plotly_dir/fname).write_text(make_plotly_html(wid,div_html,js_html), encoding="utf-8")
        log.info(f"  [PLOTLY] -> plotly/{fname}")

def extract_images(soup, out_plots_dir, dry_run):
    results=[]
    for i,img in enumerate(soup.find_all("img")):
        src=img.get("src","")
        if src.startswith("data:image"):
            m=re.match(r"data:image/([^;]+);base64,(.+)",src,re.DOTALL)
            if m:
                ext="jpg" if m.group(1).lower()=="jpeg" else m.group(1).lower()
                data=base64.b64decode(m.group(2))
                fname=f"plot_{i+1:02d}.{ext}"
                if not dry_run:
                    out_plots_dir.mkdir(parents=True, exist_ok=True)
                    (out_plots_dir/fname).write_bytes(data)
                results.append(("base64",fname))
            else: results.append(("empty",""))
        elif src and not src.startswith("http"): results.append(("relative",src))
        elif src.startswith("http"): results.append(("url",src))
        else: results.append(("empty",""))
        img.replace_with(soup.new_string(f"IMGPLACEHOLDER_{i+1}"))
    return results

def copy_relative_images(img_results, issue_path, out_plots_dir, dry_run):
    for kind,value in img_results:
        if kind!="relative": continue
        src_file=issue_path/value
        if src_file.exists():
            if not dry_run:
                out_plots_dir.mkdir(parents=True, exist_ok=True)
                shutil.copy2(src_file, out_plots_dir/src_file.name)
            log.info(f"  Copied: {value}")
        else: log.warning(f"  NOT FOUND: {value}")

def html_to_markdown(soup):
    for tag in soup(["script","style","head","nav"]): tag.decompose()
    for c in soup.find_all(string=lambda t: isinstance(t,Comment)): c.extract()
    body=soup.find("body")
    content=str(body) if body else str(soup)
    # strip= only, NOT convert= (they cannot both be set)
    md=md_lib.markdownify(content, heading_style="ATX", bullets="-", strip=["a","script","style"])
    return re.sub(r"\n{3,}","\n\n",md).strip()

def extract_title(soup):
    for tag in soup.find_all(["h1","h2","h3"]):
        text=tag.get_text(strip=True)
        if text and "Tvyal Newsletter" not in text and "Aghasi" not in text:
            return text
    return ""

BOILERPLATE_RE=[
    r"^#+ Tvyal Newsletter", r"^Tvyal Newsletter\s*$", r"Aghasi Tavadyan",
    r"English summary below", r"Հարգելի գործընկեր", r"Հուսով եմ",
    r"Ներկայացնում եմ", r"Սպասում եմ ձեզ",
    r"[Uu]nsubscribe", r"subscription", r"newsletter@", r"©\s*\d{4}",
]

def strip_boilerplate(md):
    lines,cleaned,skip=md.split("\n"),[],False
    for line in lines:
        if any(re.search(p,line) for p in BOILERPLATE_RE): skip=True; continue
        if skip and not line.strip(): skip=False; continue
        skip=False; cleaned.append(line)
    return "\n".join(cleaned).strip()

def reassemble(md, img_results, plotly_widgets):
    for i,(kind,value) in enumerate(img_results):
        ph=f"IMGPLACEHOLDER_{i+1}"
        if kind=="base64": rep=f"![](plots/{value})"
        elif kind=="relative": rep=f"![](plots/{Path(value).name})"
        elif kind=="url": rep=f"<!-- IMAGE_URL: {value} -->"
        else: rep=""
        md=md.replace(ph,rep).replace(ph.replace("_",r"\_"),rep)
    for i in range(len(plotly_widgets)):
        ph=f"PLOTLY_PLACEHOLDER_{i+1}"
        rep=(f'\n{{{{< iframe src="plotly/plotly_{i+1:02d}.html" height="520px" >}}}}\n'
             f'<!-- PLOTLY_FLAG: review plotly/plotly_{i+1:02d}.html -->\n')
        md=md.replace(ph,rep).replace(ph.replace("_",r"\_"),rep)
    return md

def make_frontmatter(folder_name, title, first_image):
    date=parse_date(folder_name); slug=folder_to_slug(folder_name)
    title_safe=title.replace('"','\\"')
    img_line=f'\nimages:\n  - "plots/{first_image}"' if first_image else ""
    return (f"---\ntitle: \"{title_safe}\"\ndate: {date}\nslug: \"{slug}\"\n"
            f"lang: hy\ndraft: false\ncategories: []\ntags: []{img_line}\n"
            f"# TODO: fill title, categories, tags with OpenCode\n---")

def process_issue(issue_path, dry_run):
    fn=issue_path.name
    result={"folder":fn,"status":"ok","warnings":[],"image_count":0,"plotly_count":0}

    html_file=find_html(issue_path)
    if not html_file:
        result["status"]="skipped_no_html"; log.warning(f"[SKIP] {fn}: no HTML"); return result

    log.info(f"Processing: {fn} ({html_file.name})")
    try: raw_html=html_file.read_text(encoding="utf-8",errors="replace")
    except Exception as e:
        result["status"]="error"; log.error(f"[ERROR] {fn}: {e}"); return result

    soup=BeautifulSoup(raw_html,"html.parser")

    year=get_year(fn); out_name=folder_to_output_name(fn)
    out_issue_dir=OUTPUT_BASE/year/out_name
    out_plots_dir=out_issue_dir/"plots"
    out_plotly_dir=out_issue_dir/"plotly"
    if not dry_run: out_issue_dir.mkdir(parents=True,exist_ok=True)

    # 1. Plotly
    pw=extract_plotly_widgets(soup)
    result["plotly_count"]=len(pw)
    if pw: result["warnings"].append(f"PLOTLY: {len(pw)}"); save_plotly_widgets(pw,out_plotly_dir,dry_run)

    # 2. Images
    imgs=extract_images(soup,out_plots_dir,dry_run)
    result["image_count"]=sum(1 for k,_ in imgs if k=="base64")
    copy_relative_images(imgs,issue_path,out_plots_dir,dry_run)

    # 3. Title
    title=extract_title(soup)
    if not title: result["warnings"].append("No title found")

    # 4. Convert & write
    final_md=reassemble(strip_boilerplate(html_to_markdown(soup)),imgs,pw)
    fm=make_frontmatter(fn,title,[v for k,v in imgs if k=="base64"][0] if any(k=="base64" for k,_ in imgs) else "")
    warn_block=("\n"+"\n".join(f"<!-- MIGRATION_WARNING: {w} -->" for w in result["warnings"])+"\n") if result["warnings"] else ""
    index_md=f"{fm}\n{warn_block}\n{final_md}\n"

    if not dry_run: (out_issue_dir/"index.md").write_text(index_md,encoding="utf-8")
    log.info(f"  -> {year}/{out_name}/index.md ({result['image_count']} imgs, {len(pw)} plotly)")
    return result

def collect_issues():
    issues=[]
    for yd in sorted(NEWSLETTER_DIR.iterdir()):
        if not yd.is_dir() or not yd.name.isdigit(): continue
        for id_ in sorted(yd.iterdir()):
            if not id_.is_dir(): continue
            if id_.name in SKIP_FOLDERS|NO_HTML_KNOWN: continue
            issues.append(id_)
    return issues

def main():
    parser=argparse.ArgumentParser()
    parser.add_argument("--issue")
    parser.add_argument("--dry-run",action="store_true")
    args=parser.parse_args()
    setup_logging()
    if args.dry_run: log.info("DRY RUN")

    if args.issue:
        p=NEWSLETTER_DIR/args.issue
        if not p.exists(): sys.exit(f"Not found: {p}")
        issues=[p]
    else:
        issues=collect_issues()
    log.info(f"{len(issues)} issues")

    sc_dir=OUTPUT_BASE/"_shortcodes"
    if not args.dry_run:
        sc_dir.mkdir(parents=True,exist_ok=True)
        (sc_dir/"iframe.html").write_text(IFRAME_SHORTCODE,encoding="utf-8")

    stats={"ok":0,"skipped":0,"error":0,"images":0,"plotly":0,"plotly_issues":[]}
    for ip in issues:
        try: r=process_issue(ip,args.dry_run)
        except Exception as e:
            log.error(f"[FATAL] {ip.name}: {e}",exc_info=True); stats["error"]+=1; continue
        if r["status"]=="ok": stats["ok"]+=1
        elif "skipped" in r["status"]: stats["skipped"]+=1
        else: stats["error"]+=1
        stats["images"]+=r["image_count"]; stats["plotly"]+=r["plotly_count"]
        if r["plotly_count"]: stats["plotly_issues"].append(r["folder"])

    log.info(f"\n{'='*50}\n  OK:{stats['ok']} Skip:{stats['skipped']} Err:{stats['error']}"
             f"\n  Images:{stats['images']} Plotly:{stats['plotly']}")
    for n in stats["plotly_issues"]: log.info(f"    [PLOTLY] {n}")
    if args.dry_run: log.info("  (dry run)")

if __name__=="__main__": main()
