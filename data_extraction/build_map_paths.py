"""Build SVG outlines of the 36 States/UTs for the Shiny map.

Input : a folder of district-level GeoJSON files, one per State/UT
        (https://github.com/udit-001/india-maps-data, commit 2884453,
        folder geojson/states/).
Output: data/map/india_map_paths.csv  - one row per State/UT with an SVG path,
        and a marker position for the very small States/UTs.
Usage : python data_extraction/build_map_paths.py <input_folder> <output_csv>
Needs : shapely (pip install shapely)

Steps: dissolve each State/UT's districts into one shape, simplify it, project
longitude/latitude onto a flat plane (x scaled by cos(22 degrees) so India is
not stretched), scale to a 1000-unit-wide drawing and write the outline as an
SVG path. Very small States/UTs also get a circle marker so they can be seen.
"""
import csv
import json
import math
import sys
from pathlib import Path

from shapely.geometry import Polygon, shape
from shapely.ops import unary_union

NAMES = {
    "andhra-pradesh": "Andhra Pradesh", "arunachal-pradesh": "Arunachal Pradesh",
    "assam": "Assam", "bihar": "Bihar", "chhattisgarh": "Chhattisgarh",
    "goa": "Goa", "gujarat": "Gujarat", "haryana": "Haryana",
    "himachal-pradesh": "Himachal Pradesh", "jharkhand": "Jharkhand",
    "karnataka": "Karnataka", "kerala": "Kerala",
    "madhya-pradesh": "Madhya Pradesh", "maharashtra": "Maharashtra",
    "manipur": "Manipur", "meghalaya": "Meghalaya", "mizoram": "Mizoram",
    "nagaland": "Nagaland", "odisha": "Odisha", "punjab": "Punjab",
    "rajasthan": "Rajasthan", "sikkim": "Sikkim", "tamil-nadu": "Tamil Nadu",
    "telangana": "Telangana", "tripura": "Tripura", "uttarakhand": "Uttarakhand",
    "uttar-pradesh": "Uttar Pradesh", "west-bengal": "West Bengal",
    "andaman-and-nicobar-islands": "A&N Islands", "chandigarh": "Chandigarh",
    "dnh-and-dd": "Dadra and Nagar Haveli and Daman and Diu",
    "delhi": "Delhi", "jammu-and-kashmir": "Jammu and Kashmir",
    "ladakh": "Ladakh", "lakshadweep": "Lakshadweep", "puducherry": "Puducherry",
}
# States/UTs too small (or too scattered) to see or hover on a country map.
MARKER_STATES = {"Goa", "Delhi", "Chandigarh", "Puducherry", "Lakshadweep",
                 "A&N Islands", "Dadra and Nagar Haveli and Daman and Diu"}
WIDTH = 1000.0
MID_LAT = 22.0
# Gaps smaller than this (square degrees) between districts are drawing
# artefacts, not real holes, so they are filled in.
MIN_HOLE_AREA = 0.05


def parts_of(geom):
    """Return the polygons that make up a (multi)polygon."""
    return list(getattr(geom, "geoms", [geom]))


def fill_small_holes(poly: Polygon) -> Polygon:
    """Remove tiny interior rings (slivers left between merged districts)."""
    keep = [h for h in poly.interiors if Polygon(h).area >= MIN_HOLE_AREA]
    return Polygon(poly.exterior, keep)


def main(src: Path, out: Path) -> None:
    shapes = {}
    for slug, name in NAMES.items():
        gj = json.loads((src / f"{slug}.geojson").read_text(encoding="utf-8"))
        geoms = [shape(f["geometry"]).buffer(0)
                 for f in gj["features"] if f.get("geometry")]
        g = unary_union(geoms)
        tol = 0.004 if name in MARKER_STATES else 0.015
        g = g.simplify(tol, preserve_topology=True)
        keep = 0.00002 if name in MARKER_STATES else 0.002
        polys = [fill_small_holes(p) for p in parts_of(g) if p.area >= keep]
        shapes[name] = polys or [fill_small_holes(p) for p in parts_of(g)]

    xs = [x for ps in shapes.values() for p in ps for x, _ in p.exterior.coords]
    ys = [y for ps in shapes.values() for p in ps for _, y in p.exterior.coords]
    lon0, lat1 = min(xs), max(ys)
    k = math.cos(math.radians(MID_LAT))
    scale = WIDTH / ((max(xs) - lon0) * k)
    height = (lat1 - min(ys)) * scale

    def proj(lon, lat):
        return (lon - lon0) * k * scale, (lat1 - lat) * scale

    def ring(coords):
        pts = [proj(x, y) for x, y in coords]
        return "M" + "L".join(f"{x:.1f} {y:.1f}" for x, y in pts[:-1]) + "Z"

    rows = []
    for name, polys in shapes.items():
        d = ""
        for p in polys:
            d += ring(p.exterior.coords)
            for hole in p.interiors:
                d += ring(hole.coords)
        if name == "Lakshadweep":      # scattered islands: mark their centre
            anchor = unary_union(polys).centroid
        else:
            anchor = max(polys, key=lambda p: p.area).representative_point()
        mx, my = proj(anchor.x, anchor.y)
        marker = name in MARKER_STATES
        rows.append({"State": name, "Path": d, "Marker": str(marker).upper(),
                     "MarkerX": f"{mx:.1f}", "MarkerY": f"{my:.1f}",
                     "Area": f"{sum(p.area for p in polys):.4f}",
                     "ViewW": f"{WIDTH:.0f}", "ViewH": f"{height:.0f}"})

    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print(f"{len(rows)} States/UTs, {out.stat().st_size / 1024:.0f} KB, "
          f"drawing {WIDTH:.0f} x {height:.0f}")


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    main(Path(sys.argv[1]), Path(sys.argv[2]))
