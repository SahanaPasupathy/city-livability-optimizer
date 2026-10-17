# Roadmap

## 1. Status (17 October 2026)

- **Interface:** the original design from the repository (commit `100fc77`), with the custom logo. UI/UX work is being taken over by another team member.
- **Added features, reachable through plain extra result tabs:** Trade-offs (Pareto efficiency on two criteria), Robustness (Livability Robustness Engine), What if (one changed rating vs your ranking), States & UTs (catalogue, data notes, comparison of up to three) and News (headlines from public RSS feeds, loaded in the background). The report includes the comparison, the what-if scenario and the Pareto set. These tabs are deliberately minimal and unstyled.
- **Kept fixes:** equal weights always listed in criterion order; the questionnaire ignores double clicks and invalid answers; unknown State names from the browser are ignored; corrected method text; data-audit notes in the report; the report no longer fails when a what-if scenario ranks a different set of States/UTs; catalogue search is case-insensitive.
- **Livability Robustness Engine:** computation complete and tested; integrated in the Robustness tab (ratings or weights, simulation count, seed, summary table with baseline rank and score next to simulated shares and rank ranges, competitive alternatives, weight influence, data notes, CSV download). Data uncertainty is deliberately not modelled because the repository has no evidence for uncertainty ranges.

## 2. Remaining work, by priority

1. **Interface (team member):** redesign the screens, including the added tabs, without changing the calculation calls; see [ARCHITECTURE.md](ARCHITECTURE.md#handover-presentation-versus-calculations). Keep the strings in [ARCHITECTURE.md](ARCHITECTURE.md#4-strings-the-r-checks-depend-on) when changing the map or report markup, and extend `tests/browser/smoke.js` as screens change.
2. **Record the data sources.** For each indicator in `data/indicator_metadata.csv`, fill in the publisher, table, URL and reporting year, and replace "not recorded in repository". Re-run `python3 tools/audit_data.py`. Only the project team (or whoever built the dataset) can do this; nothing in the repository identifies the sources.
3. **Resolve the data questions** in [DATA_AUDIT.md](DATA_AUDIT.md) once the sources are known: what the values above 100 in clean cooking fuel and immunisation measure (and whether capping at 100 is right), whether Lakshadweep's imputed Connectivity score should stay, and the outliers. Commit or document the script that produced the criterion scores. Any change to the data or the scaling changes the published rankings, so update the README results and the persona expectations in the tests together. If sources give error estimates, `data_uncertainty()` in `R/robustness.R` can use them.
4. **Decide the methodological questions**: penalising or flagging criteria built from fewer indicators (section 11 of the audit shows this decides the Retiree top 3), the direction of the inflation indicator, and the Economy indicators that disagree with each other.
5. **Run the CI workflow on GitHub** (it has only been run locally; the step that checks `docs/DATA_AUDIT.md` is current only works once the file is committed) and add an R job (for example `r-lib/actions/setup-r` and `setup-r-dependencies`, then the commands in section 4) so the app check, the testthat tests and the smoke test run on every push.
6. **Add `renv`** (`renv::init()`, commit `renv.lock`) so package versions are pinned; today only the minimum bslib version is documented.

## 3. Future features

- Indicator drill-down and a data-coverage badge for criterion scores built from fewer than all of their indicators.
- What-if with several ratings at once (`run_scenario()` already accepts several changes; the tab offers one).
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
