# Roadmap

## 1. Status (17 October 2026, after the product review)

- **Interface:** the original design from the repository with the custom logo, plus targeted functional edits (listed in [ARCHITECTURE.md](ARCHITECTURE.md#2-the-shiny-app)). The visual redesign belongs to the UI/UX teammate and is not in this repository.
- **Product review implemented** (decisions in [PRODUCT_DECISIONS.md](PRODUCT_DECISIONS.md)): reproducible score pipeline with cell status and coverage levels; rule-based limited-coverage flag; honest criterion labels and question hints; provenance in the metadata; position-based explanations with indicator evidence; relative match wording; deterministic stability check; editing answers as the what-if workflow with "what changed" and restore; plain trade-off sentences; consistent included/excluded set across features (report similar places fixed, details respect the switch); fixed 0-1 map colour scale; report aligned with the app; News off by default; tested view-model contracts for the redesign (`R/view_models.R`); an R job in CI.
- **No ranking changed.** The persona results in the README still hold.

## 2. Remaining work, by priority

1. **Interface (UI/UX teammate):** build the redesigned screens on the view-model contracts ([ARCHITECTURE.md](ARCHITECTURE.md#5-view-model-contracts)); decide whether the advanced tabs (Full ranking, Trade-offs, Robustness, What if) and the News tab stay, and where; port the functional behaviours listed in the handover; replace `tests/browser/smoke.js` with checks on stable hooks.
2. **Run the browser smoke test** on a machine with Playwright's Chromium (`npx --prefix tests/browser playwright install chromium`, then `bash tests/browser/run.sh`).
3. **Run CI on GitHub** (both jobs) and fix anything environment-specific.
4. **Data research:** the single list in [PRODUCT_DECISIONS.md](PRODUCT_DECISIONS.md#5-unresolved-research-needs-sources-or-an-owner-decision). Any change to the data or the scoring changes the published rankings, so update the README results, the persona expectations in the tests and `outputs/` together.
5. **Add `renv`** (`renv::init()`, commit `renv.lock`) so package versions are pinned.

## 3. Future features

- Indicator drill-down and a data-coverage badge for criterion scores built from fewer than all of their indicators.
- A what-if tab with several ratings at once is no longer needed: editing answers does this (`scenario_view()`).
- Exporting the simulated rank matrix (`robustness_analysis()$ranks`) if a detailed analysis needs it; the tab exports the per-State summary.

## 4. How to re-run everything

```r
install.packages(c("shiny", "bslib", "ggplot2", "testthat", "xml2", "curl"))   # once; bslib >= 0.5.0, R >= 4.0
source("analysis/01_audit.R")
source("analysis/07_map.R")           # rewrites outputs/maps/
source("analysis/08_app_check.R")
source("analysis/09_report_check.R")  # rewrites outputs/reports/ (the date in it changes)
source("analysis/11_run_tests.R")
```

```sh
python3 tools/audit_data.py --check && python3 tools/check_repo.py
python3 -m unittest discover -s tests/python
npm ci --prefix tests/browser && npx --prefix tests/browser playwright install chromium
bash tests/browser/run.sh            # smoke test of the real app on port 7654
Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'
```

`analysis/02`-`06` and `10_sensitivity.R` rewrite the tables and figures in `outputs/`; re-run them only when the data or the method changes, and review the diff (PNG files change byte-wise with package versions even when the content does not).
