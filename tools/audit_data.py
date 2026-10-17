"""R-independent data-quality audit of the livability dataset.

Reads (never writes) the dataset, the indicator metadata and the map outlines,
and produces docs/DATA_AUDIT.md: a reproducible, deterministic Markdown report.

It complements analysis/01_audit.R (which needs R). Nothing here validates the
AHP/TOPSIS code; it only checks the data those calculations start from.

Usage (from the project root, Python 3.9+, standard library only):
    python3 tools/audit_data.py                 # write docs/DATA_AUDIT.md
    python3 tools/audit_data.py --check         # also exit 1 on a structural defect
    python3 tools/audit_data.py --stdout        # print the report instead

Exit codes: 0 = no structural defect, 1 = structural defect (with --check),
2 = an input file could not be read.
"""
import argparse
import csv
import hashlib
import math
import re
import statistics
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DATA = ROOT / "data" / "State_UT_Livability_Dataset_Final.csv"
META = ROOT / "data" / "indicator_metadata.csv"
MAP = ROOT / "data" / "map" / "india_map_paths.csv"
REPORT = ROOT / "docs" / "DATA_AUDIT.md"

CRITERIA = ["Education", "Safety", "Health", "Housing",
            "Economy", "Environment", "Connectivity"]
NORM_COLS = [c + "_norm" for c in CRITERIA]
ID_COLS = ["State", "Type", "Region"]
QUALITY_COLS = ["Missing_Subindicators", "Low_Confidence"]
BASELINE_COLS = ["Baseline_Score_EqualWeights", "Baseline_Rank"]
WINSOR_P = 0.05          # the stored scores match capping at the 5th/95th percentile
TOLERANCE = 2e-4         # stored scores have 4 decimals
EXPECTED_ROWS = 36


# ---------------------------------------------------------------------------
# Reading
# ---------------------------------------------------------------------------
def read_csv(path):
    with open(path, newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        rows = list(reader)
        return reader.fieldnames or [], rows


def to_float(value):
    """'NA' or '' -> None; anything else -> float (ValueError if not numeric)."""
    if value is None:
        return None
    value = value.strip()
    if value in ("", "NA"):
        return None
    return float(value)


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


# ---------------------------------------------------------------------------
# Small statistics helpers (R-compatible where it matters)
# ---------------------------------------------------------------------------
def quantile7(values, p):
    """R's default quantile (type 7): linear interpolation between order stats."""
    s = sorted(values)
    if not s:
        raise ValueError("quantile of an empty list")
    h = (len(s) - 1) * p
    lo = int(math.floor(h))
    hi = min(lo + 1, len(s) - 1)
    return s[lo] + (h - lo) * (s[hi] - s[lo])


def rank_avg(values):
    """Average ranks (1 = smallest), as R's rank(ties.method = 'average')."""
    order = sorted(range(len(values)), key=lambda i: values[i])
    ranks = [0.0] * len(values)
    i = 0
    while i < len(order):
        j = i
        while j + 1 < len(order) and values[order[j + 1]] == values[order[i]]:
            j += 1
        for k in range(i, j + 1):
            ranks[order[k]] = (i + j) / 2 + 1
        i = j + 1
    return ranks


def spearman(x, y):
    """Spearman correlation on pairs where both values are present."""
    pairs = [(a, b) for a, b in zip(x, y) if a is not None and b is not None]
    if len(pairs) < 3:
        return None
    rx = rank_avg([a for a, _ in pairs])
    ry = rank_avg([b for _, b in pairs])
    mx, my = statistics.mean(rx), statistics.mean(ry)
    num = sum((a - mx) * (b - my) for a, b in zip(rx, ry))
    den = math.sqrt(sum((a - mx) ** 2 for a in rx) * sum((b - my) ** 2 for b in ry))
    return num / den if den else None


def scale_indicator(values, direction, cap_pct=False, p=WINSOR_P):
    """Cap at the p / 1-p quantiles, then min-max to 0-1 with higher = better."""
    vals = [None if v is None else (min(v, 100.0) if cap_pct else v) for v in values]
    present = [v for v in vals if v is not None]
    lo, hi = quantile7(present, p), quantile7(present, 1 - p)
    out = []
    for v in vals:
        if v is None:
            out.append(None)
            continue
        v = min(max(v, lo), hi)
        s = 0.0 if hi == lo else (v - lo) / (hi - lo)
        out.append(s if direction == "higher_better" else 1.0 - s)
    return out


def reconstruct_criterion(rows, indicators, meta, missing="skip", p=WINSOR_P):
    """Rebuild one criterion score from its raw indicators.

    Rule found by this audit (it reproduces the stored scores to 4 decimals):
    percentages capped at 100 -> each indicator capped at its 5th/95th
    percentile and min-max scaled with direction -> mean of the indicators that
    are present -> composite min-max rescaled to 0-1.
    Returns (scores, n_used) where scores[i] is None if no indicator is present.

    What-if options (used only to measure ranking impact, never to change data):
    missing="worst" scores a missing indicator as 0 instead of skipping it;
    p=0 turns the percentile cap off (plain min-max).
    """
    scaled = []
    for ind in indicators:
        values = [to_float(r[ind]) for r in rows]
        scaled.append(scale_indicator(values, meta[ind]["direction"],
                                      cap_pct=ind.endswith("_Pct"), p=p))
    comp, used = [], []
    for i in range(len(rows)):
        present = [s[i] for s in scaled if s[i] is not None]
        used.append(len(present))
        if missing == "worst":
            present = [0.0 if s[i] is None else s[i] for s in scaled]
        comp.append(sum(present) / len(present) if present else None)
    present = [c for c in comp if c is not None]
    lo, hi = min(present), max(present)
    scores = [None if c is None else (0.0 if hi == lo else (c - lo) / (hi - lo)) for c in comp]
    return scores, used


def ranking_impact(rows, meta, by_criterion):
    """How the persona rankings would move under other data-handling choices.

    Nothing here changes the data or the app: each scenario rebuilds the seven
    criterion scores with one documented rule changed, runs the same AHP and
    TOPSIS (tools/mcda.py), and compares the result with the stored scores.
    """
    import mcda  # same folder; imported here so the audit runs without it otherwise
    names = [r["State"] for r in rows]
    stored = [[to_float(r[c]) for c in NORM_COLS] for r in rows]

    def rebuilt(missing, p):
        cols = []
        for c in CRITERIA:
            sc, _ = reconstruct_criterion(rows, by_criterion[c], meta, missing, p)
            if any(v is None for v in sc):  # no indicator at all: median, as in the stored data
                med = statistics.median(v for v in sc if v is not None)
                sc = [med if v is None else v for v in sc]
            cols.append(sc)
        return [[cols[j][i] for j in range(len(CRITERIA))] for i in range(len(rows))]

    scenarios = [
        ("Missing indicators scored as worst (0) instead of skipped", rebuilt("worst", WINSOR_P)),
        ("No 5th/95th percentile cap (plain min-max)", rebuilt("skip", 0.0)),
    ]
    low = [r["State"] for r in rows if r["Low_Confidence"] == "1"]
    keep = [i for i, n in enumerate(names) if n not in low]
    weight_sets = [(k, mcda.ahp(v)[0]) for k, v in mcda.PERSONAS.items()]
    weight_sets.append(("Equal weights", [1 / len(CRITERIA)] * len(CRITERIA)))
    out = []
    for label, w in weight_sets:
        base, _ = mcda.ranking(names, stored, w)
        out.append((label, "Stored scores (what the app uses)", base[:3], 1.0))
        for title, X in scenarios:
            alt, _ = mcda.ranking(names, X, w)
            out.append((label, title, alt[:3], mcda.spearman_rank(base, alt)))
        sub, _ = mcda.ranking([names[i] for i in keep], [stored[i] for i in keep], w)
        base_sub = [s for s in base if s not in low]
        out.append((label, "Low-confidence States/UTs left out (app switch)", sub[:3],
                    mcda.spearman_rank(base_sub, sub)))
    return out


# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------
class Audit:
    """Collects structural checks (pass/fail) and findings for the report."""

    def __init__(self):
        self.checks = []        # (name, passed, detail)
        self.defects = []       # confirmed issues (strings)
        self.anomalies = []     # need domain review (strings)
        self.status = {}        # finding text -> (evidence status, what is needed next)
        self.info = []          # observations

    def check(self, name, passed, detail=""):
        self.checks.append((name, bool(passed), detail))
        return bool(passed)

    def add(self, kind, text, evidence, needs):
        """Record a finding with how firm the evidence is and what would resolve it."""
        getattr(self, kind).append(text)
        self.status[text] = (evidence, needs)

    @property
    def failed(self):
        return [c for c in self.checks if not c[1]]


def run_audit(data_path=DATA, meta_path=META, map_path=MAP):
    audit = Audit()
    cols, rows = read_csv(data_path)
    meta_cols, meta_rows = read_csv(meta_path)
    meta = {m["indicator"]: m for m in meta_rows}
    indicators = [m["indicator"] for m in meta_rows]
    by_criterion = {c: [m["indicator"] for m in meta_rows if m["criterion"] == c] for c in CRITERIA}
    result = {"cols": cols, "rows": rows, "meta": meta, "indicators": indicators,
              "by_criterion": by_criterion, "audit": audit}

    # -- structure ----------------------------------------------------------
    expected = ID_COLS + indicators + QUALITY_COLS + NORM_COLS + BASELINE_COLS
    audit.check("Dataset has %d rows" % EXPECTED_ROWS, len(rows) == EXPECTED_ROWS, "rows = %d" % len(rows))
    missing_cols = [c for c in expected if c not in cols]
    extra_cols = [c for c in cols if c not in expected]
    audit.check("Columns match the indicator metadata and the expected layout",
                not missing_cols and not extra_cols,
                "missing: %s; unexpected: %s" % (missing_cols or "none", extra_cols or "none"))
    audit.check("Metadata lists 17 indicators across the 7 criteria",
                len(indicators) == 17 and all(by_criterion[c] for c in CRITERIA)
                and set(m["criterion"] for m in meta_rows) == set(CRITERIA),
                "%d indicators" % len(indicators))
    audit.check("Every metadata direction is higher_better or lower_better",
                all(m["direction"] in ("higher_better", "lower_better") for m in meta_rows))
    if missing_cols:
        return result  # later checks need these columns

    names = [r["State"] for r in rows]
    dupes = sorted(set(n for n in names if names.count(n) > 1))
    audit.check("No duplicate State/UT names", not dupes, ", ".join(dupes) or "none")
    audit.check("No leading or trailing spaces in names", all(n == n.strip() for n in names))
    n_state = sum(r["Type"] == "State" for r in rows)
    n_ut = sum(r["Type"] == "UT" for r in rows)
    audit.check("Type has 28 States and 8 UTs", n_state == 28 and n_ut == 8,
                "States = %d, UTs = %d" % (n_state, n_ut))
    identical_rows = len(rows) - len(set(tuple(r[c] for c in cols[1:]) for r in rows))
    audit.check("No two States/UTs share an identical data row", identical_rows == 0)

    non_numeric = []
    for c in indicators + QUALITY_COLS + NORM_COLS + BASELINE_COLS:
        for r in rows:
            try:
                to_float(r[c])
            except ValueError:
                non_numeric.append("%s/%s" % (r["State"], c))
    audit.check("All indicator and score cells are numeric or NA", not non_numeric,
                ", ".join(non_numeric[:5]) or "none")
    if non_numeric:
        return result

    # -- missing values -----------------------------------------------------
    na_score = [(r["State"], c) for r in rows for c in NORM_COLS + BASELINE_COLS if to_float(r[c]) is None]
    audit.check("No missing criterion scores or baseline values", not na_score,
                ", ".join("%s/%s" % p for p in na_score[:5]) or "none")
    recount_bad = [r["State"] for r in rows
                   if sum(to_float(r[i]) is None for i in indicators) != int(float(r["Missing_Subindicators"]))]
    audit.check("Missing_Subindicators equals the real count of NA indicators", not recount_bad,
                ", ".join(recount_bad) or "all rows agree")
    audit.check("Low_Confidence is 0 or 1", all(r["Low_Confidence"] in ("0", "1") for r in rows))

    # -- scores -------------------------------------------------------------
    norm = {c: [to_float(r[c + "_norm"]) for r in rows] for c in CRITERIA}
    out_of_range = [c for c in CRITERIA if any(v < 0 or v > 1 for v in norm[c] if v is not None)]
    audit.check("All criterion scores lie in [0, 1]", not out_of_range, ", ".join(out_of_range) or "all in range")
    not_spanning = [c for c in CRITERIA
                    if abs(min(norm[c]) - 0) > 1e-6 or abs(max(norm[c]) - 1) > 1e-6]
    audit.check("Each criterion score spans exactly 0 to 1", not not_spanning, ", ".join(not_spanning) or "all span 0-1")
    base = [to_float(r["Baseline_Score_EqualWeights"]) for r in rows]
    gap = max(abs(statistics.mean(norm[c][i] for c in CRITERIA) - base[i]) for i in range(len(rows)))
    audit.check("Baseline score = mean of the seven criterion scores", gap < 1e-3, "largest gap %.2g" % gap)
    order = sorted(range(len(rows)), key=lambda i: -base[i])
    expected_rank = [0] * len(rows)
    for pos, i in enumerate(order):
        # ties share the best rank (R: ties.method = "min")
        expected_rank[i] = pos + 1 if pos == 0 or base[i] != base[order[pos - 1]] else expected_rank[order[pos - 1]]
    rank_bad = [rows[i]["State"] for i in range(len(rows)) if int(float(rows[i]["Baseline_Rank"])) != expected_rank[i]]
    audit.check("Baseline rank matches the baseline score", not rank_bad, ", ".join(rank_bad) or "all agree")

    # -- direction check (as analysis/01_audit.R, but from the metadata) ------
    direction_rows = []
    for ind in indicators:
        crit = meta[ind]["criterion"]
        rho = spearman([to_float(r[ind]) for r in rows], norm[crit])
        want = 1 if meta[ind]["direction"] == "higher_better" else -1
        ok = rho is not None and (rho > 0) == (want > 0)
        direction_rows.append((ind, crit, meta[ind]["direction"], rho, ok))
    bad_dir = [d[0] for d in direction_rows if not d[4]]
    audit.check("Each indicator moves its criterion score in the documented direction",
                not bad_dir, ", ".join(bad_dir) or "17 of 17")
    result["direction_rows"] = direction_rows

    # -- map outlines ---------------------------------------------------------
    if Path(map_path).exists():
        _, map_rows = read_csv(map_path)
        map_names = [m["State"] for m in map_rows]
        audit.check("Map outline names match the dataset names exactly",
                    sorted(map_names) == sorted(names),
                    "only in map: %s; only in data: %s" % (
                        sorted(set(map_names) - set(names)) or "none",
                        sorted(set(names) - set(map_names)) or "none"))
    else:
        audit.check("Map outline file exists", False, str(map_path))

    # ======================================================================
    # Findings (not structural checks)
    # ======================================================================
    profile = []
    for ind in indicators:
        vals = [to_float(r[ind]) for r in rows]
        present = [v for v in vals if v is not None]
        q1, q3 = quantile7(present, .25), quantile7(present, .75)
        iqr = q3 - q1
        extreme = [(rows[i]["State"], v) for i, v in enumerate(vals)
                   if v is not None and (v < q1 - 3 * iqr or v > q3 + 3 * iqr)]
        # Caps as used in the scaling: percentages are first capped at 100.
        pct = ind.endswith("_Pct")
        scaled_vals = [None if v is None else (min(v, 100.0) if pct else v) for v in vals]
        scaled_present = [v for v in scaled_vals if v is not None]
        lo5, hi95 = quantile7(scaled_present, WINSOR_P), quantile7(scaled_present, 1 - WINSOR_P)
        capped = [(rows[i]["State"], v) for i, v in enumerate(scaled_vals)
                  if v is not None and (v < lo5 or v > hi95)]
        m = meta[ind]
        exp_min = float(m["expected_min"]) if m["expected_min"] else None
        exp_max = float(m["expected_max"]) if m["expected_max"] else None
        outside = [(rows[i]["State"], v) for i, v in enumerate(vals) if v is not None and (
            (exp_min is not None and v < exp_min) or (exp_max is not None and v > exp_max))]
        seen = {}
        for i, v in enumerate(vals):
            if v is not None:
                seen.setdefault(v, []).append(rows[i]["State"])
        shared = {v: s for v, s in seen.items() if len(s) > 1}
        profile.append({
            "indicator": ind, "criterion": m["criterion"], "direction": m["direction"],
            "n": len(present), "missing": [rows[i]["State"] for i, v in enumerate(vals) if v is None],
            "min": min(present), "median": statistics.median(present), "max": max(present),
            "min_state": rows[vals.index(min(present))]["State"],
            "max_state": rows[vals.index(max(present))]["State"],
            "extreme": extreme, "capped": capped, "outside": outside, "shared": shared,
            "lo5": lo5, "hi95": hi95,
        })
    result["profile"] = profile

    # -- reconstruction of the stored criterion scores ------------------------
    recon = {}
    for c in CRITERIA:
        scores, used = reconstruct_criterion(rows, by_criterion[c], meta)
        diffs = [abs(s - n) for s, n in zip(scores, norm[c]) if s is not None]
        not_repro = [(rows[i]["State"], norm[c][i]) for i, s in enumerate(scores)
                     if s is None or abs(s - norm[c][i]) > TOLERANCE]
        partial = [(rows[i]["State"], used[i], len(by_criterion[c]), norm[c][i])
                   for i in range(len(rows)) if used[i] < len(by_criterion[c])]
        recon[c] = {"max_diff": max(diffs) if diffs else None, "not_repro": not_repro,
                    "partial": partial}
    result["recon"] = recon
    n_cells = len(rows) * len(CRITERIA)
    n_repro = n_cells - sum(len(r["not_repro"]) for r in recon.values())
    result["n_cells"], result["n_repro"] = n_cells, n_repro

    # -- imputed criterion scores (no indicator present) ----------------------
    imputed = []
    for c in CRITERIA:
        for st, stored in recon[c]["not_repro"]:
            idx = names.index(st)
            if all(to_float(rows[idx][i]) is None for i in by_criterion[c]):
                others = [norm[c][j] for j in range(len(rows)) if j != idx]
                imputed.append((st, c, stored, statistics.median(others)))
    result["imputed"] = imputed

    # -- low confidence --------------------------------------------------------
    missing_n = {r["State"]: int(float(r["Missing_Subindicators"])) for r in rows}
    flagged = [r["State"] for r in rows if r["Low_Confidence"] == "1"]
    by_rule = [s for s in names if missing_n[s] >= 3]
    result["low_conf"] = {"flagged": flagged, "missing_n": missing_n, "by_rule": by_rule,
                          "unflagged_missing": [(s, missing_n[s]) for s in names
                                                if missing_n[s] > 0 and s not in flagged]}
    audit.check("Low_Confidence flags exactly the States/UTs with 3 or more missing indicators",
                sorted(flagged) == sorted(by_rule),
                "flagged: %s; rule gives: %s" % (", ".join(flagged), ", ".join(by_rule)))

    # -- within-criterion agreement --------------------------------------------
    agreement = []
    for c in CRITERIA:
        inds = by_criterion[c]
        for a in range(len(inds)):
            for b in range(a + 1, len(inds)):
                x = [to_float(r[inds[a]]) for r in rows]
                y = [to_float(r[inds[b]]) for r in rows]
                sa = 1 if meta[inds[a]]["direction"] == "higher_better" else -1
                sb = 1 if meta[inds[b]]["direction"] == "higher_better" else -1
                rho = spearman([None if v is None else sa * v for v in x],
                               [None if v is None else sb * v for v in y])
                agreement.append((c, inds[a], inds[b], rho))
    result["agreement"] = agreement

    result["impact"] = ranking_impact(rows, meta, by_criterion)
    classify(result)
    return result


def on_thirds(rows, ind):
    """True if every value of an indicator is a multiple of 1/3."""
    vals = [to_float(r[ind]) for r in rows]
    return all(abs(v * 3 - round(v * 3)) < 0.01 for v in vals if v is not None)


def fmt_value(v):
    return "%.1f" % v if abs(v) >= 100 else "%.3g" % v


def classify(res):
    """Sort the findings into confirmed defects, anomalies and observations."""
    audit, prof, recon = res["audit"], {p["indicator"]: p for p in res["profile"]}, res["recon"]
    meta = res["meta"]

    undocumented = [i for i in res["indicators"] if meta[i]["source"].startswith("not recorded")]
    if undocumented:
        audit.add("defects",
            "**Sources and reporting periods are not recorded for any of the %d indicators.** "
            "The repository has no source URL, publisher, table reference or year for any indicator "
            "(see `data/indicator_metadata.csv`). The README states that four of them (%s) could not be "
            "traced to a published table; for the other %d the source is simply not written down."
            % (len(undocumented),
               ", ".join("`%s`" % i for i in res["indicators"] if meta[i]["source_traceable"].startswith("no")),
               len(undocumented) - sum(meta[i]["source_traceable"].startswith("no") for i in res["indicators"])),
            "Confirmed in the repository (no source information anywhere)",
            "Record publisher, table, URL and year for each indicator in data/indicator_metadata.csv")

    for ind in res["indicators"]:
        p = prof[ind]
        if p["outside"] and meta[ind]["expected_max"] == "100":
            audit.add("defects",
                "**`%s` exceeds 100 in %d of %d States/UTs** (maximum %.2f, %s), but its definition is not "
                "recorded. The column name says percentage, and values above 100 are not automatically wrong: "
                "coverage figures measured against an estimated target or a reference count (for example "
                "connections per 100 households) can exceed 100. Which measure this is cannot be settled "
                "without the source. Either way the scaling caps it at 100, so every State/UT at or above 100 "
                "gets the same top score on this indicator."
                % (ind, len(p["outside"]), p["n"], p["max"], p["max_state"]),
                "Confirmed: values above 100 with no recorded definition. Interpretation unverified",
                "Source verification: find the definition; then decide whether capping at 100 is right")

    audit.add("defects",
        "**The scaling of the criterion scores was not documented accurately.** The README described "
        "plain min-max scaling. The stored `_norm` columns are reproduced (to 4 decimals, %d of %d cells) "
        "only by: capping percentages at 100, capping each indicator at its 5th and 95th percentiles "
        "(R's default quantile, type 7), min-max scaling with direction, averaging the indicators that "
        "are present, and rescaling the criterion to 0-1. The README and the app text now say so."
        % (res["n_repro"], res["n_cells"]),
        "Confirmed by exact reconstruction (section 8); documentation already corrected",
        "Done: R/data_quality.R (build_criterion_scores()) rebuilds the stored scores from the raw "
        "indicators, and tests/testthat/test-data-quality.R checks every cell")

    for st, c, stored, med in res["imputed"]:
        audit.add("defects",
            "**%s's %s score (%.4f) is imputed, not measured, and this was undocumented.** None of its %s "
            "indicators are present. The stored value equals the median of the other States/UTs' %s scores "
            "(%.4f), so it was very likely median-imputed at the criterion level."
            % (st, c, stored, c, c, med),
            "Confirmed: no indicator present. Inferred: median imputation (exact match)",
            "Decision taken: the score is kept (so rankings do not change) and flagged as estimated, not "
            "measured, wherever it is shown (R/data_quality.R, coverage level \"imputed\")")

    partial_lines = []
    for c in CRITERIA:
        for st, used, total, stored in recon[c]["partial"]:
            if used > 0:
                partial_lines.append((stored, "%s %s = %.2f from %d of %d indicators" % (st, c, stored, used, total)))
    high = [t for s, t in sorted(partial_lines, reverse=True) if s >= 0.8]
    if partial_lines:
        audit.add("anomalies",
            "**Missing indicators are skipped, not penalised.** A criterion score is the mean of the indicators "
            "that are present, so a State/UT can reach the top of a criterion on partial evidence. "
            "Scores of 0.80 or more built this way: %s. This favours data-poor UTs, several of which rank highly."
            % ("; ".join(high) if high else "none"),
            "Confirmed in the data (indicator counts per score)",
            "Decision taken: scores are not penalised (rankings unchanged) but flagged: every criterion "
            "has a coverage level (full / partial / thin / imputed) shown with the score (R/data_quality.R)")

    for ind in res["indicators"]:
        p = prof[ind]
        if p["extreme"]:
            many = len(p["extreme"]) > 1
            audit.add("anomalies",
                "**Extreme value%s in `%s`:** %s (more than 3 x IQR beyond the quartiles; median %s). "
                "The 5th/95th percentile cap limits %s influence, but %s should be checked against the source."
                % ("s" if many else "", ind,
                   ", ".join("%s %s" % (s, fmt_value(v)) for s, v in p["extreme"]), fmt_value(p["median"]),
                   "their" if many else "its", "they" if many else "it"),
                "Statistical outlier; may be genuine", "Source verification")

    infl = prof.get("Avg_Inflation_PriceStability")
    if infl:
        ordered = sorted(v for v in [to_float(r["Avg_Inflation_PriceStability"]) for r in res["rows"]] if v is not None)
        audit.add("anomalies",
            "**Manipur's `Avg_Inflation_PriceStability` is %.3f**, the lowest value and %.2f below the next "
            "lowest (%.3f). Because lower is treated as better, it gives Manipur the best price-stability score. "
            "The value is unverified (as the README notes). The direction is also a methodological choice: "
            "the indicator treats any lower inflation as better, whereas price stability is often judged as "
            "closeness to a target rate (for India, the Reserve Bank's 4%% target), under which very low "
            "values would not score best." % (ordered[0], ordered[1] - ordered[0], ordered[1]),
            "Value confirmed in the data; correctness unverified",
            "Source verification, then a method decision on the indicator's direction")

    for ind in res["indicators"]:
        if on_thirds(res["rows"], ind):
            continue  # coarse values (multiples of 1/3) repeat by construction
        shared = prof[ind]["shared"]
        multi_decimal = {v: s for v, s in shared.items() if v != int(v) and len(("%r" % v).split(".")[-1]) >= 3}
        if multi_decimal:
            audit.add("anomalies",
                "**Identical values in `%s`:** %s. Matching values to three decimals can be a coincidence, "
                "but they are worth checking for a copy error."
                % (ind, "; ".join("%s share %s" % (" and ".join(s), v) for v, s in multi_decimal.items())),
                "Possible copy error; may be coincidence", "Source verification")

    neg = [(c, a, b, r) for c, a, b, r in res["agreement"] if r is not None and r < 0]
    if neg:
        audit.add("anomalies",
            "**Indicators that disagree inside a criterion** (negative Spearman correlation after "
            "orienting both as higher = better): %s. Averaging indicators that point in different "
            "directions blurs what the criterion measures."
            % "; ".join("%s: `%s` vs `%s` (%.2f)" % (c, a, b, r) for c, a, b, r in neg),
            "Confirmed in the data (correlations computed directly)",
            "Method decision: whether the criterion should combine these indicators")

    ceiling = prof.get("Institutional_Deliveries_Pct")
    if ceiling:
        near = sum(1 for r in res["rows"] if (to_float(r["Institutional_Deliveries_Pct"]) or 0) >= 99)
        audit.info.append(
            "`Institutional_Deliveries_Pct` is 99%% or more in %d of %d States/UTs, so it separates few of them."
            % (near, ceiling["n"]))

    over = [r for r in res["rows"] if (to_float(r["Clean_Cooking_Fuel_Pct"]) or 0) >= 100]
    if over:
        audit.info.append(
            "Because percentages are capped at 100 before scaling, %d of %d States/UTs share the top "
            "`Clean_Cooking_Fuel_Pct` score, so this indicator cannot separate them." % (len(over), len(res["rows"])))
    if on_thirds(res["rows"], "Education_Score"):
        audit.info.append("Every `Education_Score` value is a multiple of 1/3, which suggests an average of "
                          "three sub-scores; the components are not documented. Repeated values are therefore "
                          "expected and are not listed as anomalies.")
    lc = res["low_conf"]
    audit.info.append(
        "The low-confidence rule in `analysis/01_audit.R` is '3 or more missing indicators, or Lakshadweep'. "
        "Lakshadweep has %d missing, so the named exception is redundant with the current data."
        % lc["missing_n"].get("Lakshadweep", 0))
    audit.info.append(
        "The app now derives the flag from indicator coverage (R/data_quality.R, limited_coverage_states()): "
        "3 or more missing indicators, or a criterion with fewer than half of its indicators (or none). "
        "On the current data it flags the same four places; the others with missing indicators get a "
        "per-criterion data note.")
    if lc["unflagged_missing"]:
        audit.info.append(
            "States/UTs with missing indicators that are *not* flagged low confidence: %s."
            % ", ".join("%s (%d)" % t for t in lc["unflagged_missing"]))


# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
def rel(path):
    """Path relative to the project root when possible (for stable output)."""
    path = Path(path).resolve()
    try:
        return path.relative_to(ROOT).as_posix()
    except ValueError:
        return path.as_posix()


def fmt(v, digits=3):
    if v is None:
        return "NA"
    return "%.1f" % v if abs(v) >= 1000 else ("%." + str(digits) + "f") % v


def build_report(res, data_path=DATA, meta_path=META):
    a = res["audit"]
    L = []
    w = L.append
    w("# Data-quality audit")
    w("")
    w("<!-- Generated by tools/audit_data.py. Do not edit by hand: run "
      "`python3 tools/audit_data.py` and commit the result. -->")
    w("")
    w("This report is produced by `tools/audit_data.py` (Python standard library, no R needed). "
      "It reads the dataset without modifying it. It checks the data that the AHP/TOPSIS code "
      "starts from; it does **not** validate the R calculations themselves (see `docs/TESTING.md`).")
    w("")
    w("| Input | SHA-256 (first 16 hex) |")
    w("|---|---|")
    for p in (data_path, meta_path):
        w("| `%s` | `%s` |" % (rel(p), sha256(p)[:16]))
    w("")
    w("## Summary")
    w("")
    n_pass = sum(1 for c in a.checks if c[1])
    w("- Structural checks: **%d passed, %d failed**." % (n_pass, len(a.failed)))
    if "n_cells" in res:
        w("- Stored criterion scores reproduced from the raw indicators: **%d of %d cells** "
          "(to within %.0e)." % (res["n_repro"], res["n_cells"], TOLERANCE))
    w("- Confirmed defects: **%d**. Potential anomalies needing domain review: **%d**. "
      "Other observations: **%d**." % (len(a.defects), len(a.anomalies), len(a.info)))
    w("")
    if a.status:
        w("### Findings at a glance")
        w("")
        w("\"Confirmed\" means the issue is visible in the data or the repository itself. \"Source verification\" "
          "means only the original publisher's data can settle it; nothing here was changed to guess the answer.")
        w("")
        w("| # | Finding | Evidence | Next step |")
        w("|---|---|---|---|")
        for prefix, items in (("D", a.defects), ("A", a.anomalies)):
            for i, d in enumerate(items, 1):
                title = re.search(r"\*\*(.+?)\*\*", d)
                w("| %s%d | %s | %s | %s |" % (prefix, i, (title.group(1) if title else d[:60]).rstrip(".:"),
                                              *a.status[d]))
        w("")
        w("D = confirmed defect (section 2), A = potential anomaly (section 3).")
        w("")
    w("## 1. Structural checks")
    w("")
    w("| Check | Result | Detail |")
    w("|---|---|---|")
    for name, ok, detail in a.checks:
        w("| %s | %s | %s |" % (name, "PASS" if ok else "**FAIL**", detail.replace("|", "/")))
    w("")
    if "profile" not in res:
        return "\n".join(L).rstrip("\n") + "\n"

    w("## 2. Confirmed defects")
    w("")
    w("These follow directly from the data and the repository contents. None of them was changed in the data: "
      "the raw CSV is left exactly as it was.")
    w("")
    for i, d in enumerate(a.defects, 1):
        w("%d. %s" % (i, d))
        w("")
        w("   *Evidence:* %s. *Next step:* %s." % a.status[d])
    w("")
    w("## 3. Potential anomalies (need domain review)")
    w("")
    w("These may be genuine. They are listed so that someone with access to the sources can confirm or correct them.")
    w("")
    for i, d in enumerate(a.anomalies, 1):
        w("%d. %s" % (i, d))
        w("")
        w("   *Evidence:* %s. *Next step:* %s." % a.status[d])
    w("")
    w("## 4. Other observations")
    w("")
    for d in a.info:
        w("- %s" % d)
    w("")

    w("## 5. Indicator dictionary")
    w("")
    w("From `data/indicator_metadata.csv`. \"Inferred from column name\" means the unit is only what the column "
      "name says; nothing in the repository confirms it.")
    w("")
    w("No indicator has a written definition beyond its label and column name.")
    w("")
    w("| Indicator | Label | Criterion | Direction | Unit | Unit status | Source | Traceable | Period |")
    w("|---|---|---|---|---|---|---|---|---|")
    for ind in res["indicators"]:
        m = res["meta"][ind]
        w("| `%s` | %s | %s | %s | %s | %s | %s | %s | %s |" % (
            ind, m["label"], m["criterion"], m["direction"].replace("_", " "), m["unit"], m["unit_status"],
            m["source"], m["source_traceable"], m["reporting_period"]))
    w("")
    if all(res["meta"][i].get("transformation") for i in res["indicators"]):
        w("Transformation and missing-data handling (from the reconstruction in section 8):")
        w("")
        w("| Indicator | Transformation | Missing data |")
        w("|---|---|---|")
        for ind in res["indicators"]:
            m = res["meta"][ind]
            w("| `%s` | %s | %s |" % (ind, m["transformation"].replace(" (reconstructed by tools/audit_data.py)", ""),
                                    m["missing_data"]))
        w("")
    notes = [(ind, res["meta"][ind]["notes"]) for ind in res["indicators"] if res["meta"][ind]["notes"]]
    if notes:
        w("Notes:")
        w("")
        for ind, n in notes:
            w("- `%s`: %s" % (ind, n))
        w("")

    w("## 6. Indicator profile")
    w("")
    w("Range, missing values and the 5th/95th percentile caps used in the scaling (computed after capping "
      "percentages at 100). \"Capped\" lists the States/UTs whose value lies beyond a cap, so they share "
      "the cap's score.")
    w("")
    w("| Indicator | n | Missing | Min (State/UT) | Median | Max (State/UT) | 5th pct cap | 95th pct cap | Capped |")
    w("|---|---|---|---|---|---|---|---|---|")
    for p in res["profile"]:
        w("| `%s` | %d | %s | %s (%s) | %s | %s (%s) | %s | %s | %s |" % (
            p["indicator"], p["n"], ", ".join(p["missing"]) or "none",
            fmt(p["min"]), p["min_state"], fmt(p["median"]), fmt(p["max"]), p["max_state"],
            fmt(p["lo5"]), fmt(p["hi95"]), ", ".join(s for s, _ in p["capped"]) or "none"))
    w("")

    w("## 7. Direction check")
    w("")
    w("Spearman correlation between each raw indicator and its criterion score. Higher-is-better indicators "
      "should correlate positively, lower-is-better negatively (same idea as check 12 in `analysis/01_audit.R`).")
    w("")
    w("| Indicator | Criterion | Direction | Spearman | OK |")
    w("|---|---|---|---|---|")
    for ind, crit, d, rho, ok in res["direction_rows"]:
        w("| `%s` | %s | %s | %s | %s |" % (ind, crit, d.replace("_", " "), fmt(rho, 2), "yes" if ok else "**no**"))
    w("")

    w("## 8. Reconstruction of the criterion scores")
    w("")
    w("Rule tested: percentages capped at 100; each indicator capped at its 5th/95th percentile (type 7) and "
      "min-max scaled with direction; mean of the indicators present; criterion rescaled to 0-1.")
    w("")
    w("| Criterion | Indicators | Largest difference | Cells not reproduced | Rows built from fewer indicators |")
    w("|---|---|---|---|---|")
    for c in CRITERIA:
        r = res["recon"][c]
        w("| %s | %d | %s | %s | %s |" % (
            c, len(res["by_criterion"][c]), "%.1e" % r["max_diff"] if r["max_diff"] is not None else "NA",
            ", ".join("%s (stored %.4f)" % t for t in r["not_repro"]) or "none",
            ", ".join("%s (%d of %d, score %.2f)" % t for t in r["partial"]) or "none"))
    w("")

    w("## 9. Low-confidence States/UTs")
    w("")
    lc = res["low_conf"]
    w("Flagged in the data: %s. The flag matches the rule \"3 or more of the 17 indicators missing\"." %
      ", ".join(lc["flagged"]))
    w("")
    w("| State/UT | Missing indicators | Flagged |")
    w("|---|---|---|")
    for st, n in sorted(lc["missing_n"].items(), key=lambda t: (-t[1], t[0])):
        if n > 0:
            w("| %s | %d | %s |" % (st, n, "yes" if st in lc["flagged"] else "no"))
    w("")

    w("## 10. Agreement between indicators of the same criterion")
    w("")
    w("Spearman correlation after orienting both indicators as higher = better. Low or negative values "
      "mean the indicators measure different things.")
    w("")
    w("| Criterion | Indicator A | Indicator B | Spearman |")
    w("|---|---|---|---|")
    for c, x, y, rho in res["agreement"]:
        w("| %s | `%s` | `%s` | %s |" % (c, x, y, fmt(rho, 2)))
    w("")

    w("## 11. Potential impact on rankings (what-if, not applied)")
    w("")
    w("Each scenario rebuilds the criterion scores with one rule changed and re-runs the same AHP + TOPSIS "
      "(Python version in `tools/mcda.py`). The app and the data are **not** changed; this only shows how "
      "sensitive the published top 3s are to the data-handling choices found above. Spearman compares the "
      "full ranking with the one from the stored scores (for the low-confidence row, over the 32 remaining).")
    w("")
    w("| Weights | Scenario | Top 3 | Spearman vs stored |")
    w("|---|---|---|---|")
    for label, title, top, rho in res["impact"]:
        w("| %s | %s | %s | %.3f |" % (label, title, ", ".join(top), rho))
    w("")
    w("## 12. What this means for the rankings")
    w("")
    w("- Rankings depend on indicators whose sources and years are not documented, so they cannot be "
      "independently re-derived from primary data yet.")
    ret = {t[1]: t[2] for t in res["impact"] if t[0] == "Retiree"}
    worst_key = [k for k in ret if k.startswith("Missing indicators")][0]
    assert set(ret[worst_key]).isdisjoint({"Lakshadweep", "Ladakh"}), "update the Retiree sentence below"
    w("- **The Retiree top 3 depends on how missing data are handled.** Scoring missing indicators as worst "
      "instead of skipping them (section 11) replaces Lakshadweep and Ladakh, both low-confidence UTs, with "
      "Andhra Pradesh and Kerala. Other top 3s change in at most two places; the full-ranking correlation "
      "with the stored scores is at least %.2f in every scenario." % min(t[3] for t in res["impact"]))
    w("- Removing the percentile cap changes several top 3s (for example, Lakshadweep would lead for the "
      "Family persona), so the cap is a consequential, previously undocumented modelling choice.")
    w("- Data-poor UTs can score highly on criteria where they lack indicators, because missing indicators "
      "are skipped. Use the app's \"leave out low-confidence States/UTs\" switch when comparing them.")
    w("- Percentile capping means differences beyond the 5th/95th percentiles do not count (for example, "
      "Delhi's PM2.5 is treated like the 95th-percentile value).")
    w("- None of these findings changes the AHP/TOPSIS calculations. Changing the data or the scaling rule is a "
      "methodological decision for the project team and would change the published rankings.")
    w("")
    return "\n".join(L).rstrip("\n") + "\n"


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--check", action="store_true", help="exit 1 if a structural check fails")
    parser.add_argument("--stdout", action="store_true", help="print the report instead of writing it")
    parser.add_argument("--output", type=Path, default=REPORT, help="where to write the report")
    args = parser.parse_args(argv)
    try:
        res = run_audit()
        report = build_report(res)
    except (OSError, KeyError) as exc:
        print("Could not read the input files: %s" % exc, file=sys.stderr)
        return 2
    if args.stdout:
        sys.stdout.write(report)
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(report, encoding="utf-8")
        print("Wrote %s" % rel(args.output))
    a = res["audit"]
    print("Structural checks: %d passed, %d failed; %d confirmed defects, %d anomalies."
          % (len(a.checks) - len(a.failed), len(a.failed), len(a.defects), len(a.anomalies)),
          file=sys.stderr)
    for name, _, detail in a.failed:
        print("FAIL: %s (%s)" % (name, detail), file=sys.stderr)
    return 1 if (args.check and a.failed) else 0


if __name__ == "__main__":
    sys.exit(main())
