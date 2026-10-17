"""Tests for the R-independent tools in tools/ (standard library unittest).

Run from the project root:  python3 -m unittest discover -s tests/python -v

These test the Python audit and consistency tools only. They do not test the
R code; see docs/TESTING.md for the R tests.
"""
import csv
import shutil
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "tools"))

import audit_data  # noqa: E402
import check_repo  # noqa: E402
import mcda  # noqa: E402


class StatisticsHelpers(unittest.TestCase):
    def test_quantile_matches_r_type_7(self):
        # R: quantile(c(1, 2, 3, 4, 10), c(.05, .5, .95)) -> 1.2, 3, 8.8
        x = [10, 1, 3, 2, 4]
        self.assertAlmostEqual(audit_data.quantile7(x, 0.05), 1.2)
        self.assertAlmostEqual(audit_data.quantile7(x, 0.5), 3.0)
        self.assertAlmostEqual(audit_data.quantile7(x, 0.95), 8.8)
        self.assertEqual(audit_data.quantile7([7], 0.3), 7)

    def test_spearman(self):
        self.assertAlmostEqual(audit_data.spearman([1, 2, 3, 4], [10, 20, 30, 40]), 1.0)
        self.assertAlmostEqual(audit_data.spearman([1, 2, 3, 4], [4, 3, 2, 1]), -1.0)
        # missing values are dropped pairwise; ties get average ranks
        self.assertAlmostEqual(audit_data.spearman([1, None, 2, 2, 3], [1, 5, 2, 2, 3]), 1.0)
        self.assertIsNone(audit_data.spearman([1, 2], [1, 2]))

    def test_scale_indicator_caps_and_orients(self):
        vals = [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 1000]
        up = audit_data.scale_indicator(vals, "higher_better")
        down = audit_data.scale_indicator(vals, "lower_better")
        self.assertEqual(up[-1], 1.0)             # the outlier is capped, not stretched
        self.assertEqual(up[0], 0.0)
        for a, b in zip(up, down):
            self.assertAlmostEqual(a + b, 1.0)
        self.assertEqual(audit_data.scale_indicator([None, 1, 2, 3], "higher_better")[0], None)

    def test_percentages_capped_at_100(self):
        s = audit_data.scale_indicator([50, 90, 100, 120, 150], "higher_better", cap_pct=True, p=0)
        self.assertEqual(s[2:], [1.0, 1.0, 1.0])

    def test_formatting_helpers(self):
        self.assertEqual(audit_data.fmt(None), "NA")
        self.assertEqual(audit_data.fmt(1508.9), "1508.9")
        self.assertEqual(audit_data.fmt_value(1067.031), "1067.0")


class AuditOnRealData(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.res = audit_data.run_audit()

    def test_structural_checks_pass(self):
        self.assertEqual(self.res["audit"].failed, [])

    def test_scores_are_reproduced_except_the_imputed_cell(self):
        self.assertEqual(self.res["n_cells"], 252)
        self.assertEqual(self.res["n_repro"], 251)
        self.assertEqual([(s, c) for s, c, _, _ in self.res["imputed"]], [("Lakshadweep", "Connectivity")])
        _, _, stored, median = self.res["imputed"][0]
        self.assertAlmostEqual(stored, median, places=4)

    def test_known_findings_are_reported(self):
        defects = " ".join(self.res["audit"].defects)
        self.assertIn("Clean_Cooking_Fuel_Pct", defects)
        self.assertIn("Immunisation_Pct", defects)
        self.assertIn("not recorded", defects)
        anomalies = " ".join(self.res["audit"].anomalies)
        self.assertIn("Manipur", anomalies)

    def test_every_finding_says_how_firm_it_is(self):
        a = self.res["audit"]
        for text in a.defects + a.anomalies:
            evidence, needs = a.status[text]
            self.assertTrue(evidence and needs, text)
        needs_source = [t for t in a.anomalies if "Source verification" in a.status[t][1]]
        self.assertTrue(needs_source)  # outliers and possible copy errors go to the source

    def test_report_is_deterministic(self):
        self.assertEqual(audit_data.build_report(self.res), audit_data.build_report(audit_data.run_audit()))

    def test_raw_data_is_not_modified(self):
        before = audit_data.sha256(audit_data.DATA)
        audit_data.run_audit()
        self.assertEqual(before, audit_data.sha256(audit_data.DATA))


class AuditCatchesInjectedDefects(unittest.TestCase):
    """Each test breaks a copy of the data on purpose and expects a failed check."""

    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp())
        self.data = self.tmp / "data.csv"
        self.meta = self.tmp / "meta.csv"
        shutil.copy(audit_data.DATA, self.data)
        shutil.copy(audit_data.META, self.meta)

    def tearDown(self):
        shutil.rmtree(self.tmp)

    def edit(self, path, fn):
        with open(path, newline="", encoding="utf-8") as fh:
            reader = csv.DictReader(fh)
            fields, rows = reader.fieldnames, list(reader)
        fn(rows)
        with open(path, "w", newline="", encoding="utf-8") as fh:
            w = csv.DictWriter(fh, fieldnames=fields)
            w.writeheader()
            w.writerows(rows)

    def failed_names(self):
        res = audit_data.run_audit(self.data, self.meta, audit_data.MAP)
        return " | ".join(name for name, _, _ in res["audit"].failed)

    def test_duplicate_state(self):
        self.edit(self.data, lambda rows: rows[1].update(State=rows[0]["State"]))
        self.assertIn("No duplicate State/UT names", self.failed_names())

    def test_score_out_of_range(self):
        self.edit(self.data, lambda rows: rows[0].update(Health_norm="1.5"))
        self.assertIn("All criterion scores lie in [0, 1]", self.failed_names())

    def test_missing_count_wrong(self):
        self.edit(self.data, lambda rows: rows[0].update(Missing_Subindicators="0"))
        self.assertIn("Missing_Subindicators", self.failed_names())

    def test_non_numeric_cell(self):
        self.edit(self.data, lambda rows: rows[0].update(Crime_Rate="n/a"))
        self.assertIn("numeric", self.failed_names())

    def test_wrong_direction_in_metadata(self):
        def flip(rows):
            for r in rows:
                if r["indicator"] == "AirQuality_PM25":
                    r["direction"] = "higher_better"
        self.edit(self.meta, flip)
        self.assertIn("documented direction", self.failed_names())

    def test_baseline_rank_wrong(self):
        self.edit(self.data, lambda rows: rows[0].update(Baseline_Rank="1"))
        self.assertIn("Baseline rank", self.failed_names())


class RankingImpact(unittest.TestCase):
    def test_worst_case_scores_missing_indicators_as_zero(self):
        meta = {"a": {"direction": "higher_better"}, "b": {"direction": "higher_better"}}
        rows = [{"a": "0", "b": "0"}, {"a": "10", "b": "NA"}, {"a": "5", "b": "10"}]
        skip, used = audit_data.reconstruct_criterion(rows, ["a", "b"], meta, "skip", 0)
        worst, _ = audit_data.reconstruct_criterion(rows, ["a", "b"], meta, "worst", 0)
        self.assertEqual(used, [2, 1, 2])
        self.assertEqual(skip[1], 1.0)        # skipped: scored on its one indicator
        self.assertLess(worst[1], skip[1])    # worst case: the missing one counts as 0

    def test_impact_table_covers_every_weight_set_and_scenario(self):
        res = audit_data.run_audit()
        self.assertEqual(len(res["impact"]), 16)
        for label, title, top, rho in res["impact"]:
            self.assertEqual(len(top), 3)
            self.assertTrue(-1 <= rho <= 1)
            if title.startswith("Stored"):
                self.assertEqual(rho, 1.0)

    def test_spearman_rank(self):
        self.assertEqual(mcda.spearman_rank(list("abcd"), list("abcd")), 1.0)
        self.assertEqual(mcda.spearman_rank(list("abcd"), list("dcba")), -1.0)


class RepoChecks(unittest.TestCase):
    def test_ahp_equal_answers_give_equal_weights(self):
        w, cr = check_repo.ahp([3] * 7)
        for x in w:
            self.assertAlmostEqual(x, 1 / 7)
        self.assertAlmostEqual(cr, 0.0)

    def test_ahp_saaty_difference_rule(self):
        w, cr = check_repo.ahp([5, 4, 3, 3, 2, 2, 1])
        self.assertEqual(w, sorted(w, reverse=True))
        self.assertLess(cr, 0.10)

    def test_topsis_hand_case(self):
        # Same toy case as tests/testthat/test-topsis.R
        cl = check_repo.topsis([[1, 1], [0, 0], [1, 0]], [0.5, 0.5])
        self.assertEqual([round(c, 9) for c in cl], [1.0, 0.0, 0.5])

    def test_broken_links_are_found(self):
        tmp = Path(tempfile.mkdtemp())
        try:
            (tmp / "ok.md").write_text("x", encoding="utf-8")
            doc = tmp / "doc.md"
            doc.write_text("[a](ok.md) [b](missing.md) [c](https://example.org) [d](#top)\n"
                           "```\n[e](also-missing.md)\n```\n", encoding="utf-8")
            broken = check_repo.broken_links([doc])
            self.assertEqual(len(broken), 1)
            self.assertIn("missing.md", broken[0])
        finally:
            shutil.rmtree(tmp)

    def test_r_vector_parsing(self):
        src = 'X <- c("a", "b",\n  "c")\nexpected_sign = c(1, -1,  # note\n 1)'
        self.assertEqual(check_repo.r_string_vector(src, "X"), ["a", "b", "c"])
        self.assertEqual(check_repo.r_number_vector(src, "expected_sign"), [1.0, -1.0, 1.0])


if __name__ == "__main__":
    unittest.main()
