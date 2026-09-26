import sys
from pathlib import Path
from tempfile import TemporaryDirectory
import unittest
import xml.etree.ElementTree as ET

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
from clean_sitemap import clean, NAMESPACE, SITE


class SitemapTest(unittest.TestCase):
    def test_filters_and_keeps_valid_xml(self):
        with TemporaryDirectory() as directory:
            path = Path(directory) / "sitemap.xml"
            path.write_text(f"""<urlset xmlns='{NAMESPACE}'>
<url><loc>{SITE}404.html</loc></url>
<url><loc>{SITE}CONTRIBUTE.html</loc></url>
<url><loc>{SITE}LICENSE-text.html</loc></url>
<url><loc>{SITE}LICENSE.html</loc></url>
<url><loc>{SITE}index.html</loc></url>
<url><loc>{SITE}reference/ROC.html</loc></url></urlset>""")
            self.assertEqual(clean(path), (2, 4))
            self.assertTrue(path.read_bytes().startswith(b'<?xml version="1.0" encoding="UTF-8"?>'))
            self.assertEqual([e.text for e in ET.parse(path).findall(f".//{{{NAMESPACE}}}loc")],
                             [f"{SITE}index.html", f"{SITE}reference/ROC.html"])
            self.assertEqual(clean(path), (2, 0))

    def test_rejects_wrong_host(self):
        with TemporaryDirectory() as directory:
            path = Path(directory) / "sitemap.xml"
            path.write_text(f"<urlset xmlns='{NAMESPACE}'><url><loc>https://evil.example/a</loc></url></urlset>")
            with self.assertRaises(ValueError):
                clean(path)


if __name__ == "__main__":
    unittest.main()
