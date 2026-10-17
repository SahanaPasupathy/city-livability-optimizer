# City Livability Optimizer

**Personalized State and UT Livability Recommendation System**
Programming for Data Science (BCSE207L), built in R.

**Team:** Saipriya R (24BDS1100) and Sahana Pasupathy (24BDS1049)

A person answers seven short questions about what matters to them. The system turns those answers into personal weights (AHP), ranks all 36 Indian States and Union Territories for them (TOPSIS), shows the top 3 with a plain-language explanation, and lets them explore the result on an interactive map of India.

---

## 1. Problem

Livability rankings usually give one fixed list. But a young professional, a family and a retiree do not want the same things. This project asks:

> Which State or UT is the best fit for *this* person's priorities, and why?

## 2. Data

`data/State_UT_Livability_Dataset_Final.csv` has one row per State/UT (36 rows: 28 States, 8 UTs).

- **17 raw indicators** are grouped into **7 criteria**:

| Criterion | Built from |
|---|---|
| Education | Education score, Grade 8 proficiency, higher-secondary enrolment |
| Safety | Crime rate, murders per lakh |
| Health | Immunisation, institutional deliveries, health-worker density |
| Housing | Kachha houses, clean cooking fuel, rural piped water |
| Economy | Unemployment, poverty headcount (MPI), price stability |
| Environment | Air quality (PM2.5) |
| Connectivity | Villages with 3G/4G, electric vehicles per lakh people |

- Each criterion score runs from **0 to 1 with higher = better**, so different units can be compared. The stored scores were built as follows (reconstructed by `tools/audit_data.py`, which reproduces 251 of the 252 stored values to 4 decimals): percentages capped at 100, each indicator capped at its 5th and 95th percentiles and min-max scaled with its direction, the indicators of a criterion averaged (missing ones skipped), and the result rescaled to 0-1.
- 4 States/UTs are flagged **low data confidence** because 3 or more of their 17 indicators are missing: A&N Islands, Chandigarh, Ladakh, Lakshadweep.
- Lakshadweep has no Connectivity indicator; its Connectivity score equals the median of the other States/UTs, so it appears to be imputed.
- Indicator units, directions, sources and reporting periods are listed in [`data/indicator_metadata.csv`](data/indicator_metadata.csv). **Sources and years are not recorded in the repository for any indicator.** The full data-quality audit is in [`docs/DATA_AUDIT.md`](docs/DATA_AUDIT.md).

## 3. Method

1. **Questionnaire:** the user rates each criterion from 1 (not important) to 5 (extremely important).
2. **AHP (Analytic Hierarchy Process):**
   - For each pair of criteria, the difference between the two answers becomes a comparison on Saaty's 1-9 scale (0 apart = 1, 1 apart = 3, 2 = 5, 3 = 7, 4 = 9).
   - The weights are the principal eigenvector of the 7x7 comparison matrix.
   - The consistency ratio is CR = CI / RI with RI(7) = 1.32. CR of 0.10 or below is accepted.
3. **TOPSIS:**
   - The weighted scores are v = w x score.
   - The ideal solution takes the best value on each criterion and the worst solution takes the lowest.
   - Closeness = D- / (D+ + D-), from 0 (worst) to 1 (ideal).
4. **Explanation:** "Why this fits you" compares a State's scores on the user's three heaviest criteria with the other States (Strong match is 0.70 or more, Moderate 0.40 to 0.70, Falls short below 0.40), and names where it falls furthest short.
5. **Similar States/UTs:** Euclidean distance on the 7 scores with equal importance. This is kept separate from the personalised ranking.

## 4. Results (demonstration personas)

Importance answers (1-5) for three personas: Young Professional, Family and Retiree. Baseline = the same TOPSIS with equal weights.

| Persona | Top 3 | CR | Spearman vs equal weights | Top-3 overlap with equal weights |
|---|---|---|---|---|
| Young Professional | Delhi, Chandigarh, Goa | 0.023 | 0.812 | 2 of 3 |
| Family | Himachal Pradesh, Chandigarh, Punjab | 0.014 | 0.834 | 2 of 3 |
| Retiree | Lakshadweep, Ladakh, Karnataka | 0.039 | 0.675 | 0 of 3 |
| Equal weights (baseline) | Chandigarh, Himachal Pradesh, Goa | 0 | 1 | 3 of 3 |

- **Personalisation changes the answer.** For the Retiree persona, Delhi falls 19 places and Andhra Pradesh rises 14.
- **Low-confidence States rank high** (Chandigarh, Ladakh, Lakshadweep appear in several top 3s). The app has a switch to leave them out; for the Retiree persona the top 3 then becomes Karnataka, Andhra Pradesh, Kerala.
- Tables are in `outputs/tables/` and figures in `outputs/figures/`.

### Robustness (Step 9, `analysis/10_sensitivity.R`)

How much do the recommendations change if the weights or the method change a little?

- **Random weight changes** (1000 draws per persona; each weight moves by about 0.02-0.06). The number 1 choice stays the same in 82% of draws for Young Professional, 40% for Family, 68% for Retiree and 51% for equal weights. The exact top 3 stays the same in 77%, 47%, 59% and 22% of draws. The average Spearman correlation with the original ranking is 0.92 to 0.97. So **the overall ranking is stable, but the order inside the top 3 is not**: Delhi (99%), Lakshadweep (97%) and Ladakh (93%) almost always stay in the top 3, while third place (for example Punjab 64%, Karnataka 67%) often swaps with the next State.
- **Ignoring one criterion at a time.** Ranking correlation with the original stays above 0.65 in every case. Some criteria matter a lot for some people: ignoring Connectivity changes the Young Professional top 3 to Manipur, Delhi, Tripura, ignoring Education or Safety changes the Family top 3, and ignoring Environment or Health changes the Retiree top 3.
- **Leaving out the 4 low-confidence States/UTs** hardly changes the order of the others (Spearman 0.995 to 1.000), but it does change the top 3 of the Retiree persona (Lakshadweep and Ladakh drop out).
- **Different method.** A simple weighted sum agrees with TOPSIS (Spearman 0.97 to 0.99) and gives the same top 3 as a set in 3 of 4 cases. Vector normalisation also agrees (0.975 to 0.994).
- Figure: `outputs/figures/10_top3_stability.png`. Tables: `outputs/tables/10_*.csv`.

## 5. The Shiny app

Features: flashcard questions, three presets (Young Professional, Family, Retiree), a skip option (equal weights), the top 3 with "Why this fits you", an interactive India map coloured by closeness, a searchable and sortable full ranking, a weights panel with the consistency check, "How it works" and "About and limits" tabs, and a downloadable personal report (HTML). The welcome screen shows the project logo (`app/www/img/logo.svg`), which is also the browser-tab icon.

The interface is the original one; UI/UX work is being taken over by another team member. Features developed since are reachable through plain extra tabs on the results screen, in the original style:

- **Trade-offs:** choose two criteria; a scatter plot marks the Pareto-efficient States/UTs (no other place is at least as good on both and better on one) and explains where your #1 stands (`R/pareto.R`, `R/atlas.R`).
- **Robustness:** re-runs your ranking under slightly different ratings or weights (simulation count and seed selectable) and shows, next to your actual rank and match score, how often each place is ranked 1st or in the top 3, 5 and 10, its median simulated rank with a 5-95% range, competitive alternatives and data notes; the full table downloads as CSV (`R/robustness.R`).
- **What if:** change one rating and compare the new ranking with yours (`R/scenario.R`).
- **States & UTs:** all 36 with search, current rank and score, data notes (missing indicators) per State/UT, and a comparison of up to three (`R/catalogue.R`).
- **News:** a few recent livability headlines from public RSS feeds (The Hindu, Mongabay India), loaded in the background; external news, never used in the rankings (`R/news.R`). `LIVABILITY_NEWS=off` switches it off.

The downloaded report then also includes the comparison, the what-if scenario (if one is set) and the Pareto-efficient places. See [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) for how presentation and calculations are separated.

### How to run it

1. Install R 4.0 or newer (the code uses raw strings) from <https://cloud.r-project.org> (or `brew install r` on macOS) and, optionally, RStudio. No system libraries beyond a C/C++ compiler are needed if R builds packages from source (Homebrew R does; on macOS the Xcode Command Line Tools provide it).
2. Install the packages once. The app needs **bslib 0.5.0 or newer** (for `nav_panel()`). xml2 and curl are only needed by the News tab and its tests (the tab says news is unavailable and the tests are skipped without them):

   ```r
   install.packages(c("shiny", "bslib", "ggplot2", "testthat", "xml2", "curl"))
   ```

   xml2 and curl are needed for the News tab (without them it says news is unavailable); callr (installed with testthat) runs the background news refresh; httpuv (installed with shiny) is used by the news tests.

3. Start the app from the project root:

   ```sh
   Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'
   ```

   or open `city-livability-optimizer.Rproj` in RStudio, open `app/app.R` and click **Run App**. The app finds the data from either the project root or `app/`.

Verified on macOS (arm64) with R 4.6.1, shiny 1.14.0, bslib 0.12.0, ggplot2 4.0.3, htmltools 0.5.9, testthat 3.3.2, xml2 1.6.0 and curl 8.1.0 (callr, used for the background news fetch, is installed with testthat).

### How to reproduce the analysis

Source the scripts in `analysis/` in order from the project root, for example `source("analysis/04_topsis.R")`. Each prints PASS/FAIL checks and writes its tables and figures to `outputs/`.

| Script | What it does |
|---|---|
| `01_audit.R` | Audits the dataset before use |
| `02_eda.R` | Exploratory analysis and baseline ranking |
| `03_ahp.R` | Persona weights and consistency |
| `04_topsis.R` | Personalised TOPSIS rankings |
| `05_recommend.R` | Top 3, explanations and similar States |
| `06_compare.R` | Personalised vs equal-weight ranking |
| `07_map.R` | Interactive map files (`outputs/maps/`) |
| `08_app_check.R` | Checks of the app's logic |
| `09_report_check.R` | Checks of the report and texts |
| `10_sensitivity.R` | Sensitivity and robustness analysis |
| `11_run_tests.R` | Runs the automated tests in `tests/testthat/` |

### Automated tests

**R tests** (need R). Run `source("analysis/11_run_tests.R")` (needs `install.packages("testthat")`). The 67 tests (391 expectations) cover:

- AHP: the difference rule, a perfectly consistent matrix, Saaty's textbook 3x3 example and contradictory answers.
- TOPSIS: a small case checked by hand and rejection of bad inputs.
- Similar States/UTs: nearest-distance results.
- The real data and the app's calculation: the three persona top 3s, the dataset's shape, and the low-confidence switch.
- Pareto efficiency: hand-checked cases, ties, direction of preference, missing values, a brute-force cross-check on random data, invariance to rescaling, and that every dominated place on the real data has a valid dominator.
- Livability Robustness Engine: reproduces the baseline ranking exactly when nothing is perturbed, matches `rank_stability()` in weights mode, reproducible seeds, top-3/5/10 counts and rank quantiles recomputed from the simulated ranks, exact enumeration of all rating shifts with Monte Carlo agreeing within sampling error, ties, missing scores, documented data-uncertainty bounds only, invalid inputs and zero weights.
- What-if scenarios: equal to a fresh run of the engine, rank changes, a new #1, determinism, invalid ratings, and the report with a scenario that ranks a different set of States/UTs.
- The Trade-off Atlas drawing (36 points, the real Pareto set, works without the stylesheet, follows the low-confidence switch).
- Catalogue data, per-State data notes and the report's optional sections agree with the ranking; news parsing (two date formats, unsafe links dropped), filtering, caching and switching off; news retrieval from a local web server (good feed, 404, malformed XML, unreachable host) and the background refresh with no duplicate processes.

Breaking the code on purpose makes tests fail, so they do catch mistakes. The check scripts `analysis/04`-`10` add further PASS/FAIL checks, including that `app/app.R` loads (`08_app_check.R`) and that the report is complete (`09_report_check.R`).

**Browser smoke test of the real app** (need R with the packages above, and Node.js). `tests/browser/run.sh` starts the app, drives it in headless Chromium (logo and title, questionnaire including a double click, presets, equal weights, results and weights checked against values computed in R, ranking search and sort, map selection, the low-confidence switch, report download with its optional sections, invalid inputs sent straight to the server, and the Trade-offs, Robustness, What if, States & UTs and News tabs, again checked against R), then stops it. It checks that the app and its calculations work, not the visual design:

```sh
npm ci --prefix tests/browser
npx --prefix tests/browser playwright install chromium
bash tests/browser/run.sh            # exit code 0 = every check passed
```

**Checks that run without R** (Python 3.9+ standard library; Node.js for the R syntax check). They check the data, the documentation and the committed results, but they do **not** test the R code:

```sh
python3 tools/audit_data.py --check            # data audit, writes docs/DATA_AUDIT.md
python3 tools/check_repo.py                     # metadata vs R code, links, committed results
python3 -m unittest discover -s tests/python    # tests for the two tools above
npm ci --ignore-scripts --prefix tools/r-syntax && node tools/r-syntax/check.js   # R syntax only
```

A GitHub Actions workflow (`.github/workflows/checks.yml`) is set up to run these on every push and pull request; it has not run on GitHub yet. See [`docs/TESTING.md`](docs/TESTING.md) for the full inventory and what each check does and does not prove.

### Testing status

Last full run: 17 October 2026, R 4.6.1 on macOS (arm64), after the added features were connected to the original interface.

| Area | Status |
|---|---|
| Calculations (AHP, TOPSIS, Pareto, robustness, what-if, explanations) | 67 testthat tests (391 expectations) pass; `analysis/01`-`10` checks pass. |
| Shiny app (`app/app.R`, `app/www/`) | Started and driven in headless Chromium: 47 smoke checks pass (`tests/browser/`), including numbers compared with R. `analysis/08_app_check.R`: 29/29. |
| Map and report (`R/map.R`, `R/report.R`) | `analysis/07_map.R` 15/15, `09_report_check.R` 16/16; `outputs/maps/` and `outputs/reports/` regenerated and inspected. |
| Data, documentation, committed results | Python audit and repository checks pass (see [`docs/TESTING.md`](docs/TESTING.md)). |
| Not tested | Visual design and accessibility of the interface (being handed over), browsers other than Chromium, deployment to a Shiny server. The CI workflow has not run on GitHub yet. |

## 6. Project structure

```text
app/                 Shiny app (app.R; www/style.css, www/table.js,
                     www/img/logo.svg)
R/                   Tested functions: common, ahp, topsis, similarity,
                     explain, compare, map, app_logic, app_text, report,
                     pareto, atlas, catalogue, news, robustness, scenario,
                     sensitivity
analysis/            Numbered scripts (01-11)
data/                Dataset, indicator_metadata.csv, map outlines (data/map/)
data_extraction/     build_map_paths.py: how the map outlines were made
docs/                Architecture, data audit, testing, roadmap
tools/               Checks that run without R (Python, Node.js)
tests/testthat/      Automated R tests (testthat)
tests/python/        Tests for the Python tools
tests/browser/       Browser smoke test of the running app (Playwright)
outputs/             Tables, figures, maps, sample report (written by the
                     analysis/ scripts)
```

More documentation:

- [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md): how the app, the R functions and the checks fit together, and the design system.
- [`docs/DATA_AUDIT.md`](docs/DATA_AUDIT.md): generated data-quality audit (confirmed defects and open questions).
- [`docs/TESTING.md`](docs/TESTING.md): every test and check, and what each one proves.
- [`docs/ROADMAP.md`](docs/ROADMAP.md): validation steps that need R, remaining work and future features.

## 7. Limitations

- The ranking is **relative, not absolute**. A closeness of 0.78 means "closest to the best available State/UT", not "78% livable".
- The weights come from simple 1-5 answers. They express priorities, not measured facts.
- **Four indicators have sources that could not be traced** to a published table: Education score, Crime rate, Price stability and Electric vehicles per lakh. Criteria that use them should be treated with some caution.
- **Some values look unusual.** Manipur's price-stability figure is 1.32, far below other States, and has not been verified.
- **Low-confidence States/UTs** can rank high on thin data (see Results). Missing indicators are skipped, not penalised, so for example Chandigarh's Housing score of 1.00 rests on one of its three indicators.
- **Sources and reporting years are not recorded** for any indicator, and two "percentage" indicators exceed 100 (clean cooking fuel in 22 States/UTs, immunisation in 7). See [`docs/DATA_AUDIT.md`](docs/DATA_AUDIT.md).
- **Percentile capping** means differences beyond the 5th and 95th percentiles of an indicator do not affect the score.
- **Results depend on data-handling choices.** If missing indicators counted as the worst score instead of being skipped, the Retiree top 3 would lose Lakshadweep and Ladakh; without the percentile cap, Lakshadweep would lead for the Family persona. These what-if results are in [`docs/DATA_AUDIT.md`](docs/DATA_AUDIT.md) (section 11); the app itself is unchanged.
- **Inflation direction.** Lower inflation always counts as better, so Manipur's very low value earns the best price-stability score. Judging price stability as closeness to a target rate would treat it differently; this is an open methodological choice.
- The rankings are not objective or definitive: they depend on the indicators chosen, their (undocumented) definitions and sources, the scaling, the weights you give and the TOPSIS method.
- Data are at State/UT level; cities and districts can differ a lot from the State average.
- **Map outlines** come from the public repository `udit-001/india-maps-data` (commit 2884453), whose licence is not clearly stated. They are simplified, for illustration only, and are not an authoritative map. The outline file is rebuilt by `data_extraction/build_map_paths.py`.
