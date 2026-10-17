"""Repository consistency checks that run without R (Python 3.9+, standard library).

What it checks:
  1. The indicator metadata agrees with the R code (R/common.R and
     analysis/01_audit.R): same 17 indicators, same 7 criteria, same directions.
  2. Every file the Shiny app loads from app/www/ exists.
  3. Relative links in README.md and docs/*.md point to files that exist.
  4. The data caveats quoted in R/app_text.R still match the dataset.
  5. docs/DATA_AUDIT.md is up to date with tools/audit_data.py, and the Python
     audit agrees with what the R audit recorded (outputs/tables/01_audit_checks.csv)
     about the same dataset.
  6. The committed result tables (outputs/tables/03, 04) and the README results
     table agree with an independent Python re-computation of the documented
     method (AHP difference rule + principal eigenvector, TOPSIS without extra
     normalisation).

Check 6 shows that the committed outputs and README are consistent with the
documented method. It does NOT test the R code: only the R tests
(analysis/08_app_check.R, 09_report_check.R, tests/testthat) can do that.

Usage:  python3 tools/check_repo.py        (exit 1 if any check fails)
"""
import csv
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import audit_data  # noqa: E402  (same folder)
import mcda  # noqa: E402

ROOT = audit_data.ROOT
PERSONAS = mcda.PERSONAS

results = []


def check(name, ok, detail=""):
    results.append((name, bool(ok), detail))


def r_string_vector(src, var):
    """Strings inside `var <- c(...)` in R source."""
    m = re.search(r"\b%s\s*<-\s*c\((.*?)\)" % re.escape(var), src, re.S)
    return re.findall(r'"([^"]+)"', m.group(1)) if m else None


def r_number_vector(src, var):
    m = re.search(r"\b%s\s*=\s*c\((.*?)\)" % re.escape(var), src, re.S)
    if not m:
        return None
    body = re.sub(r"#.*", "", m.group(1))
    return [float(x) for x in re.findall(r"-?\d+(?:\.\d+)?", body)]


# ---------------------------------------------------------------------------
# 1. Metadata vs R code
# ---------------------------------------------------------------------------
def check_metadata():
    _, meta = audit_data.read_csv(audit_data.META)
    common = (ROOT / "R" / "common.R").read_text(encoding="utf-8")
    audit_r = (ROOT / "analysis" / "01_audit.R").read_text(encoding="utf-8")
    r_ind = r_string_vector(common, "INDICATORS")
    r_crit = r_string_vector(common, "CRITERIA")
    check("Metadata indicators match INDICATORS in R/common.R (same order)",
          r_ind == [m["indicator"] for m in meta], "R: %s" % (len(r_ind) if r_ind else "not found"))
    check("Metadata criteria match CRITERIA in R/common.R",
          r_crit == audit_data.CRITERIA and set(m["criterion"] for m in meta) == set(r_crit or []))
    signs = r_number_vector(audit_r, "expected_sign")
    meta_signs = [1.0 if m["direction"] == "higher_better" else -1.0 for m in meta]
    check("Metadata directions match expected_sign in analysis/01_audit.R",
          signs == meta_signs, "R signs found: %s" % (len(signs) if signs else "none"))
    ahp = (ROOT / "R" / "ahp.R").read_text(encoding="utf-8")
    ok = True
    for name, vals in PERSONAS.items():
        m = re.search(r'"%s"\s*=\s*c\((.*?)\)' % re.escape(name), ahp, re.S)
        found = [int(x) for x in re.findall(r"=\s*(\d)", m.group(1))] if m else None
        ok = ok and found == vals
    check("Persona answers used here match PERSONAS in R/ahp.R", ok)


# ---------------------------------------------------------------------------
# 2. App assets
# ---------------------------------------------------------------------------
def check_app_assets():
    app = (ROOT / "app" / "app.R").read_text(encoding="utf-8")
    refs = sorted(set(re.findall(r'(?:href|src)\s*=\s*"([^":]+\.(?:css|js))"', app)))
    missing = [r for r in refs if not (ROOT / "app" / "www" / r).exists()]
    check("Every stylesheet/script the app loads exists in app/www/", refs and not missing,
          "referenced: %s; missing: %s" % (", ".join(refs), ", ".join(missing) or "none"))


# ---------------------------------------------------------------------------
# 3. Markdown links
# ---------------------------------------------------------------------------
LINK = re.compile(r"(?<!!)\[[^\]]*\]\(([^)\s]+)(?:\s+\"[^\"]*\")?\)")


def broken_links(files):
    """Relative Markdown links (outside code fences) whose target file is missing."""
    broken = []
    for f in files:
        text = re.sub(r"```.*?```", "", Path(f).read_text(encoding="utf-8"), flags=re.S)
        for target in LINK.findall(text):
            if re.match(r"^(https?:|mailto:|#)", target):
                continue
            path = target.split("#", 1)[0]
            if path and not (Path(f).parent / path).resolve().exists():
                broken.append("%s -> %s" % (audit_data.rel(f), target))
    return broken


def check_markdown_links():
    files = [ROOT / "README.md"] + sorted((ROOT / "docs").glob("*.md"))
    broken = broken_links(files)
    check("Relative links in README.md and docs/*.md resolve", not broken, "; ".join(broken) or
          "%d files checked" % len(files))


# ---------------------------------------------------------------------------
# 4. Data caveats quoted in the app text
# ---------------------------------------------------------------------------
def check_text_caveats():
    _, rows = audit_data.read_csv(audit_data.DATA)
    text = (ROOT / "R" / "app_text.R").read_text(encoding="utf-8")
    manipur = next(r for r in rows if r["State"] == "Manipur")
    check("Manipur's price-stability value quoted in R/app_text.R matches the data",
          ("%.2f" % float(manipur["Avg_Inflation_PriceStability"])) in text)
    low = [r["State"] for r in rows if r["Low_Confidence"] == "1"]
    check("Every low-confidence State/UT is named in the limitations text",
          all(s in text for s in low), ", ".join(low))
    laks = next(r for r in rows if r["State"] == "Lakshadweep")
    check("Lakshadweep's Connectivity score quoted in R/app_text.R matches the data",
          ("%.2f" % float(laks["Connectivity_norm"])) in text)
    over_ccf = sum(float(r["Clean_Cooking_Fuel_Pct"]) > 100 for r in rows)
    over_imm = sum(float(r["Immunisation_Pct"]) > 100 for r in rows)
    check("Counts of percentages above 100 quoted in R/app_text.R match the data",
          ("in %d States/UTs" % over_ccf) in text and ("immunisation in %d" % over_imm) in text,
          "clean cooking %d, immunisation %d" % (over_ccf, over_imm))


# ---------------------------------------------------------------------------
# 5. Generated audit report is current
# ---------------------------------------------------------------------------
def check_audit_report():
    res = audit_data.run_audit()
    expected = audit_data.build_report(res)
    current = audit_data.REPORT.read_text(encoding="utf-8") if audit_data.REPORT.exists() else ""
    check("docs/DATA_AUDIT.md is up to date (run python3 tools/audit_data.py)", expected == current)
    check("Every audit finding has an evidence status and a next step",
          all(t in res["audit"].status for t in res["audit"].defects + res["audit"].anomalies))

    # The R audit (analysis/01_audit.R) wrote its own counts for the same CSV.
    _, r_checks = audit_data.read_csv(ROOT / "outputs" / "tables" / "01_audit_checks.csv")
    detail = {c["Check"].split(".")[0]: c["Detail"] for c in r_checks}
    rows, inds = res["rows"], res["indicators"]
    n_na = sum(audit_data.to_float(r[i]) is None for r in rows for i in inds)
    over = {p["indicator"]: len(p["outside"]) for p in res["profile"] if p["outside"]}
    want_na = "%d of %d cells" % (n_na, len(rows) * len(inds))
    want_over = "; ".join("%s: %d" % (k, v) for k, v in over.items())
    check("Python audit matches the R audit's recorded counts (missing cells, percentages > 100)",
          detail.get("6b", "").startswith(want_na) and detail.get("9") == want_over,
          "R: '%s' / '%s'; Python: '%s' / '%s'" % (detail.get("6b"), detail.get("9"), want_na, want_over))
    # The audit's what-if code must reproduce the R sensitivity result it overlaps with.
    _, comp = audit_data.read_csv(ROOT / "outputs" / "tables" / "10_method_comparison.csv")
    r_low = {c["Set"]: c["Alt_Top3"].split(" > ") for c in comp if c["Test"] == "Low-confidence left out"}
    py_low = {t[0]: t[2] for t in res["impact"] if t[1].startswith("Low-confidence")}
    check("Audit's ranking-impact code reproduces the R sensitivity table (low-confidence left out)",
          r_low == py_low, "; ".join("%s: %s" % (k, ", ".join(v)) for k, v in py_low.items()))
    r_fail = [c["Check"] for c in r_checks if c["Status"] == "FAIL"]
    check("The committed R audit result has no FAIL", not r_fail, ", ".join(r_fail) or "%d checks" % len(r_checks))


# ---------------------------------------------------------------------------
# 6. Independent re-computation of the documented results
# ---------------------------------------------------------------------------
ahp = mcda.ahp
topsis = mcda.topsis


def check_results():
    _, rows = audit_data.read_csv(audit_data.DATA)
    names = [r["State"] for r in rows]
    X = [[float(r[c]) for c in audit_data.NORM_COLS] for r in rows]
    sets = {k: ahp(v) for k, v in PERSONAS.items()}
    sets["Equal weights"] = ([1 / 7] * 7, 0.0)

    _, cons = audit_data.read_csv(ROOT / "outputs" / "tables" / "03_persona_consistency.csv")
    cr_ok = all(abs(float(c["CR"]) - sets[c["Persona"]][1]) < 1e-6 for c in cons)
    check("Persona consistency ratios in outputs/tables/03 match a re-computation", cr_ok)

    _, ranks = audit_data.read_csv(ROOT / "outputs" / "tables" / "04_topsis_rankings.csv")
    table = {r["State"]: r for r in ranks}
    worst_gap, rank_bad, top3 = 0.0, [], {}
    for k, (w, _) in sets.items():
        cl = topsis(X, w)
        order = sorted(range(len(names)), key=lambda i: -cl[i])
        top3[k] = [names[i] for i in order[:3]]
        for i, st in enumerate(names):
            worst_gap = max(worst_gap, abs(float(table[st]["%s - Closeness" % k]) - cl[i]))
            rank = 1 + sum(c > cl[i] for c in cl)
            if int(table[st]["%s - Rank" % k]) != rank:
                rank_bad.append("%s/%s" % (k, st))
    check("Closeness and ranks in outputs/tables/04 match a re-computation",
          worst_gap < 1e-4 and not rank_bad,
          "largest closeness gap %.1e; rank mismatches: %s" % (worst_gap, ", ".join(rank_bad) or "none"))

    readme = (ROOT / "README.md").read_text(encoding="utf-8")
    missing = []
    for k, t in top3.items():
        label = "Equal weights (baseline)" if k == "Equal weights" else k
        line = next((l for l in readme.splitlines() if l.startswith("| %s |" % label)), "")
        if ", ".join(t) not in line:
            missing.append("%s: %s" % (k, ", ".join(t)))
        if k != "Equal weights" and ("%.3f" % sets[k][1]) not in line:
            missing.append("%s CR %.3f" % (k, sets[k][1]))
    check("README results table shows the re-computed top 3 and CR for each persona", not missing,
          "; ".join(missing) or "4 rows agree")


def main():
    for fn in (check_metadata, check_app_assets, check_markdown_links, check_text_caveats,
               check_audit_report, check_results):
        try:
            fn()
        except Exception as exc:  # report the failing check instead of a traceback
            check(fn.__name__, False, "error: %s" % exc)
    width = max(len(n) for n, _, _ in results)
    for name, ok, detail in results:
        print("[%s] %s%s" % ("PASS" if ok else "FAIL", name.ljust(width),
                             ("  -  " + detail) if detail else ""))
    failed = [r for r in results if not r[1]]
    print("\n%d passed, %d failed" % (len(results) - len(failed), len(failed)))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
