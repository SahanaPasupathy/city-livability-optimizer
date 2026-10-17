# Testing and checks

This project has three layers of checks:

1. **R tests and check scripts** test the actual calculations, the app's logic and the report. They need R.
2. **Browser tests** start the real Shiny app and drive it in headless Chromium. They need R and Node.js.
3. **R-independent checks** (Python, Node.js) test the data, the documentation and the committed results. They run anywhere, including in CI, but **they do not test the R code**.

Last run: 17 October 2026, R 4.6.1 (macOS arm64), shiny 1.14.0, bslib 0.12.0, after the product review changes (see [PRODUCT_DECISIONS.md](PRODUCT_DECISIONS.md)). Every check listed below passed **except the browser smoke test, which was updated but not run** (see section 2).

## 1. R tests (need R)

### Automated tests: `tests/testthat/` (85 tests, 526 expectations)

Run with `source("analysis/11_run_tests.R")` or `testthat::test_dir("tests/testthat")` from the project root.

| File | Tests | What they check |
|---|---|---|
| `test-ahp.R` | 7 | difference rule, a perfectly consistent matrix, Saaty's 3x3 textbook example, contradictory answers flagged, equal answers give equal weights, bad input rejected, every 1-5 answer set has CR at or below 0.10 |
| `test-topsis.R` | 5 | hand-calculated case, weights rescaled and order-independent, heavier weight helps, closeness in [0, 1] on real data, bad inputs rejected |
| `test-similarity.R` | 5 | nearest by Euclidean distance, never similar to itself, `k` capped, symmetry, unknown names rejected |
| `test-explain-and-app.R` | 8 | ordinal numbers, rank change sign, the three persona top 3s, dataset shape, the app calculation and incomplete answers, the low-confidence switch, explanations quote real numbers, reproducible random weights |
| `test-pareto.R` | 10 | hand-checked two-criterion case, ties (identical points both efficient), equal-on-one-better-on-other dominates, direction of preference (higher, lower, mixed), missing values not classified and never dominating, single criterion, brute-force cross-check on 20 random data sets, every dominated place on the real data has a valid dominator, invariance to rescaling, frontier order, bad inputs |
| `test-catalogue-report.R` | 5 | catalogue ranks, scores and order equal the ranking; left-out States keep a row without a rank; data notes from the dataset (Lakshadweep, Kerala); the report's sections (comparison with trade-offs, what-if, per-place data notes, evidence, stability, checklist, no Pareto list) leave the 36 ranking rows intact; the report's similar places come from the places ranked (regression) |
| `test-scenario.R` | 5 | a scenario equals a fresh run of the engine; rank change = baseline rank - scenario rank; new #1 detected; unchanged ratings give no change; deterministic; same low-confidence setting as the baseline; invalid ratings and incomplete answers rejected; the report includes a scenario that ranks a different set of States/UTs (this failed before the fix) |
| `test-atlas.R` | 2 | 36 focusable points, efficient points equal `pareto_front()`, coloured without the stylesheet, one selected point, labels with real scores; follows the low-confidence switch; `atlas_assets()` |
| `test-robustness.R` | 14 | no perturbation reproduces the baseline TOPSIS ranking exactly; `p_change = 0` and direct weights also do; fixed seeds reproduce and different seeds differ; Dirichlet mode equals `rank_stability()`; top-1/3/5/10 counts, shares, mean and quantiles recomputed from the simulated ranks; exact enumeration of all 3^7 rating shifts and Monte Carlo within 4 standard errors of it; clipping to 1-5 through AHP; ties share the best rank; missing scores reported as not comparable; data notes change no number; data uncertainty only from documented bounds and separable from preference uncertainty; invalid inputs and zero weights; competitive alternatives and weight influence recomputed directly; run time |
| `test-data-quality.R` | 7 | the pipeline rebuilds all 252 stored scores; Lakshadweep's Connectivity is the only imputed score and equals the median; cell status (21 missing, 29 capped at 100, Delhi PM2.5 capped) with raw values untouched; direction, ties, the 100 cap and switching the percentile cap off; coverage levels (Chandigarh Housing thin, Lakshadweep Connectivity imputed); the limited-coverage rule matches the stored flag and triggers on a thin criterion alone; metadata provenance (the four untraced indicators, no invented sources) and criterion scope texts |
| `test-view-models.R` | 10 | stability check counts recomputed by hand, 14/7 variants, determinism, no probability wording, exclusion respected; `scenario_view()` with several changes equals a fresh run, no-change and invalid input; `recommendation_view()` equals the engine, evidence quotes the raw data, alternative top 3 without limited-coverage places; position verdicts; `comparison_view()` trade-offs (one-sided pair found with `pareto_front()`), input errors; the same included/excluded set in comparison, details, similar places, catalogue, stability and robustness (regression); `place_detail()` standing sentence; fixed 0-1 map colour scale; the weights message |
| `test-news.R` | 7 | RSS parsing (both date formats, CDATA, unsafe links dropped), topic filter and recency, bad input, cache round-trip and expiry, off by default / switching on and overriding feeds; `fetch_news()` against a local web server (good feed, 404, malformed XML, unreachable host); the background refresh writes the cache and never runs twice at once; no internet used |

Deliberate bugs show the tests catch errors: changing the dominance rule (`>=` to `>`) makes 24 Pareto expectations fail; an off-by-one in the robustness top-k share (`<=` to `<`) makes 10 robustness expectations fail.

### Check scripts: `analysis/`

Each prints `[PASS]`/`[FAIL]` lines. Source them from the project root.

| Script | Focus |
|---|---|
| `01_audit.R` | dataset structure, missing values, flags, ranges, scaling direction, baseline (writes `outputs/tables/01_audit_checks.csv`) |
| `04_topsis.R` | TOPSIS results |
| `05_recommend.R` | top 3, explanations, similar States/UTs |
| `06_compare.R` | personalised vs equal-weight ranking |
| `07_map.R` | map outlines, colours, badges, low-confidence outlines (writes `outputs/maps/`) |
| `08_app_check.R` | app logic, low-confidence switch, map with exclusions, **`app/app.R` loads and builds a Shiny app** |
| `09_report_check.R` | report content and HTML escaping (writes `outputs/reports/`) |
| `10_sensitivity.R` | robustness analysis |

(`02_eda.R` and `03_ahp.R` produce tables and figures.)

## 2. Browser smoke test of the real app

`bash tests/browser/run.sh [screenshot_dir]` starts the app on port 7654 (`APP_PORT` to change it), runs `tests/browser/smoke.js` in headless Chromium and stops the app. Setup once: `npm ci --prefix tests/browser` and `npx --prefix tests/browser playwright install chromium` (Playwright 1.59.1).

**Not run on 17 October 2026 after the product review:** Playwright's Chromium is not installed on this machine, and installing it was out of scope. `smoke.js` was updated for the intended changes (relative match to 2 decimals, stability and alternative notes, the weights message, catalogue details, News off by default, the report's data-notes section) and passes `node --check`. The server logic it exercises was instead driven with `shiny::testServer` (17 checks, all passed). The checks were last all green before the review.

The 48 checks cover: the original title and the custom logo (also the tab icon); that no redesigned element is present; the questionnaire, including a double click that must answer only one question; results, match scores, weights and consistency ratio compared with values computed in R; the result tabs; ranking search and sort; map selection; the low-confidence switch; report download with the comparison, what-if and Pareto sections; a preset and equal weights; unknown State names and invalid answers sent straight to the server; Trade-offs (Pareto sets for two criterion pairs equal to R, same-criterion message); Robustness (default and non-default settings equal to R, baseline rank and score shown apart from simulated ranks, data notes, CSV with 36 rows, displayed top-3 shares agree with the CSV so a share that occurred is never shown as 0%); What if (Safety 5 equals R: Lakshadweep becomes #1); States & UTs (36 rows, search, no-match message, data notes, comparison scores); News (real headlines with safe external links, or the unavailable message); and no JavaScript or Shiny errors. It tests that the app and its calculations work, not the visual design, which is being handed over.

The earlier suites for the redesigned interface (`e2e.js`, `map.js`, `features.js`, `invalid_input.js`, `news.js`) were moved out of the repository with the redesign; a copy is kept outside the repository.

## 3. Checks without R

All need only Python 3.9+ with the standard library, except the R syntax check, which needs Node.js.

| Command | What it checks | What it does **not** prove |
|---|---|---|
| `python3 tools/audit_data.py --check` | 19 structural checks on the dataset, metadata and map names; regenerates [`DATA_AUDIT.md`](DATA_AUDIT.md) (profiles, defects, anomalies, reconstruction of the stored scores). Exit 1 on a structural defect. | That the data are correct at source: sources are not recorded. |
| `python3 tools/check_repo.py` | metadata matches `R/common.R` and `analysis/01_audit.R`; persona answers match `R/ahp.R`; app assets exist; Markdown links resolve; numbers quoted in `R/app_text.R` match the data; `DATA_AUDIT.md` is current; the Python audit agrees with the R audit's recorded counts and its what-if code reproduces the R sensitivity table; **`outputs/tables/03` and `04` and the README results table match an independent Python re-computation of the documented AHP + TOPSIS method**. | That the R functions are correct. It shows the *committed outputs* agree with the documented method. |
| `python3 -m unittest discover -s tests/python -v` | 25 tests of the two tools, including tests that break a copy of the data on purpose (duplicate name, score out of range, wrong count, non-numeric cell, flipped direction, wrong rank) and expect a failure. | Anything about the R code. |
| `npm ci --ignore-scripts --prefix tools/r-syntax` then `node tools/r-syntax/check.js` | Every `.R` file under `app/`, `R/`, `analysis/`, `tests/` parses with the tree-sitter R grammar. | That the code runs: missing functions, wrong arguments or package versions are not detected. |
| `node --check app/www/table.js tests/browser/smoke.js` | JavaScript syntax. | Behaviour in a browser. |
| `npx markdownlint-cli2` | Markdown style of `README.md` and `docs/` (rules in `.markdownlint-cli2.jsonc`). | Content accuracy. |

## 4. Continuous integration

`.github/workflows/checks.yml` has two jobs, run on every push and pull request:

- `checks`: all of section 3 (no R).
- `r-tests` (added in the product review): installs R and the packages with `r-lib/actions`, runs the testthat suite (fails on any failure or error) and `analysis/01`, `07`, `08`, `09` (fails if any line says `[FAIL]`). It does not run the browser smoke test.

**Neither job has run on GitHub yet.** Each step was run locally with the same commands, and the workflow file parses as YAML.

## 5. Results of the last run (17 October 2026, after the product review)

| Command | Result |
|---|---|
| `source("analysis/11_run_tests.R")` | 85 tests, 526 expectations passed, 0 failed (baseline before the changes: 67 / 391, all passed) |
| `source("analysis/01_audit.R")` | all checks passed (`outputs/tables/01_audit_checks.csv` unchanged) |
| `source("analysis/07_map.R")` | 15 passed, 0 failed (`outputs/maps/` regenerated: fixed colour scale and legend) |
| `source("analysis/08_app_check.R")` | 29 passed, 0 failed (includes: `app/app.R` loads and builds a Shiny app) |
| `source("analysis/09_report_check.R")` | 17 passed, 0 failed (`outputs/reports/` regenerated) |
| `shiny::testServer()` on `app/` (ad hoc, not committed) | 17 checks passed: questionnaire to results, top 3 notes, what changed and restore, details with the switch on, report, every tab |
| `Rscript -e 'shiny::runApp("app")'` | starts and serves the page and stylesheet |
| `python3 tools/audit_data.py --check` | 19 structural checks passed; `docs/DATA_AUDIT.md` regenerated |
| `python3 tools/check_repo.py` | 18 passed, 0 failed |
| `python3 -m unittest discover -s tests/python` | 25 tests OK |
| `node tools/r-syntax/check.js` | 45 of 45 R files parse |
| `node --check` on `app/www/*.js`, `tests/browser/*.js` | OK |
| `bash tests/browser/run.sh` | **not run** (no Chromium for Playwright) |
| `analysis/02`-`06`, `10` | not re-run: they rewrite figures and tables only, and no score or ranking changed |
