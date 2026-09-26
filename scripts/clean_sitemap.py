"""Remove non-search pages from pkgdown's generated sitemap."""

from pathlib import Path
from urllib.parse import urlsplit
import sys
import xml.etree.ElementTree as ET

NAMESPACE = "http://www.sitemaps.org/schemas/sitemap/0.9"
SITE = "https://laresbernardo.github.io/lares/"
SKIP = {"404.html", "CONTRIBUTE.html", "LICENSE.html", "LICENSE-text.html"}


def clean(path: Path) -> tuple[int, int]:
    ET.register_namespace("", NAMESPACE)
    tree = ET.parse(path)
    root = tree.getroot()
    if root.tag != f"{{{NAMESPACE}}}urlset":
        raise ValueError("not a sitemap urlset")
    kept = removed = 0
    for entry in list(root):
        if entry.tag != f"{{{NAMESPACE}}}url":
            raise ValueError("unexpected sitemap element")
        loc = entry.findtext(f"{{{NAMESPACE}}}loc")
        if not loc or not loc.startswith(SITE):
            raise ValueError(f"unexpected URL: {loc}")
        parsed = urlsplit(loc)
        if parsed.query or parsed.fragment:
            raise ValueError(f"noncanonical URL: {loc}")
        if parsed.path.removeprefix("/lares/") in SKIP:
            root.remove(entry)
            removed += 1
        else:
            kept += 1
    xml = ET.tostring(root, encoding="utf-8", short_empty_elements=True)
    path.write_bytes(b'<?xml version="1.0" encoding="UTF-8"?>\n' + xml)
    return kept, removed


if __name__ == "__main__":
    if len(sys.argv) != 2:
        raise SystemExit("usage: clean_sitemap.py docs/sitemap.xml")
    kept, removed = clean(Path(sys.argv[1]))
    print(f"Sitemap: {kept} canonical URLs kept, {removed} non-search pages removed")
