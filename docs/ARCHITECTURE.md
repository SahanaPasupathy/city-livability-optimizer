# Architecture

How the project fits together, for anyone changing the app, the analysis or the checks.

## 1. Overview

```text
data/State_UT_Livability_Dataset_Final.csv   (36 States/UTs x 17 indicators + 7 criterion scores)
data/indicator_metadata.csv                  (indicator dictionary; see docs/DATA_AUDIT.md)
        |
        v
R/common.R      load_livability_data(), criteria_matrix()      -> 36 x 7 decision matrix
R/ahp.R         run_ahp(answers)                               -> weights, CR
R/topsis.R      topsis(X, weights)                             -> closeness, rank
R/explain.R     priority_breakdown(), shortfall_sentence(), explain_state(), by_weight()
R/similarity.R  similar_states()
R/compare.R     rank_change(), top_movers(), top_k_overlap()
R/app_logic.R   compute_recommendation()  (AHP + TOPSIS + low-confidence switch), weights_table()
R/map.R         render_india_map()        (SVG map, no extra packages)
R/report.R      build_report_html()       (downloadable report)
R/app_text.R    METHOD_STEPS, LIMITATIONS, DATA_NOTES, ROBUSTNESS_NOTES (shared text)
R/sensitivity.R dirichlet_weights(), rank_stability(), rank_without() (analysis/10_sensitivity.R)
R/robustness.R  robustness_analysis()     (Livability Robustness Engine)
R/pareto.R      pareto_front(), explain_pareto()
R/catalogue.R   catalogue_data(), state_data_notes(), load_indicator_metadata()
R/news.R        parse_rss(), select_news(), fetch_news(), ensure_news_refresh(), cache helpers
R/scenario.R    run_scenario()            (what-if: changed ratings vs baseline)
R/atlas.R       render_atlas(), atlas_assets()  (Pareto scatter plot, presentation only)
        |
        +--> app/app.R            Shiny interface (original design)
        +--> analysis/01-11       numbered scripts: checks, tables, figures, maps
        +--> tests/testthat/      automated R tests
```

The rule that keeps this maintainable: **all calculations live in `R/`**. `app/app.R` only decides which screen to show and reacts to input; the analysis scripts and the tests call the same functions, so the app and the published results cannot disagree.

## 2. The Shiny app

The interface is the original one from the repository (commit `100fc77`): welcome screen, seven flashcard questions, results with tabs. Styles are in `app/www/style.css`; `app/www/table.js` provides search and sorting for the ranking table. Interface work is being taken over by another team member.

Result tabs: Top 3, Map, Full ranking, My weights (original), then Trade-offs, Robustness, What if, States & UTs, News (added with minimal, unstyled controls), then How it works, About and limits (original).

Changes to the original parts of `app/app.R`, none of which change how they look:

- The project logo (`app/www/img/logo.svg`) replaces the emoji on the welcome screen and is the browser-tab icon. The title is unchanged.
- Each answer button sends the question it belongs to (`{step, value}`); the server ignores an answer from a screen that has already been replaced (a double click used to answer the next question too) and accepts only whole numbers 1-5.
- `input$map_selected` is accepted only if it is the name of a State/UT.
- The report download also passes the comparison, the what-if scenario (if its rating differs) and the Pareto set to `build_report_html()`.

### Added tabs: where the calculations come from

| Tab | Inputs | Server output | Calculation (R/) |
|---|---|---|---|
| Trade-offs | `atlas_x`, `atlas_y` | `output$atlas` | `pareto_front()`, `explain_pareto()`; drawn by `render_atlas()` |
| Robustness | `rb_method`, `rb_level`, `rb_n`, `rb_seed`, `rb_run`; `rb_csv` download | `output$robustness`, `output$rb_csv` | `robustness_analysis()`, `dirichlet_weight_sd()` |
| What if | `wi_crit`, `wi_value` | `output$whatif` | `run_scenario()` (uses `compute_recommendation()`, `rank_change()`) |
| States & UTs | `cat_search`, `cat_pick`, `cmp_pick` | `output$catalogue`, `output$cat_detail`, `output$compare` | `catalogue_data()`, `state_data_notes()`, `explain_pareto()` |
| News | none | `output$news` | `read_news_cache()`, `ensure_news_refresh()` (one background `callr` process for all sessions, polled for up to 25 s; falls back to a cache up to 48 h old or an "unavailable" message) |

Every tab reads the same `rec()` (AHP + TOPSIS for the current answers and the low-confidence switch), so ranks and scores agree across tabs and the report. The robustness settings map "How sure are you?" to `p_change` 0.10 / 0.25 / 0.50 (ratings) or concentration 200 / 50 / 15 (weights); these are assumptions the user chooses, documented in the tab.

### Handover: presentation versus calculations

| Safe to redesign freely (presentation) | Do not change without the calculation tests (logic) |
|---|---|
| `app/www/style.css`, `app/www/table.js`, `app/www/img/` | `R/common.R`, `R/ahp.R`, `R/topsis.R`, `R/app_logic.R`, `R/explain.R`, `R/similarity.R`, `R/compare.R`, `R/sensitivity.R` |
| The UI functions in `app/app.R` (`welcome_screen()`, `question_screen()`, `top_card()`, `ranking_table()`, `weights_panel()`, the `*_panel()` functions, `results_screen()`, `ui`) and the markup inside the `render*` blocks | `R/robustness.R`, `R/pareto.R`, `R/scenario.R`, `R/catalogue.R`, `R/news.R`, `R/app_text.R` (texts quoted by checks) |
| `R/atlas.R` (drawing only; keep `data-state`/`data-tip` if the map script should keep working) | The calculation calls inside the `server` function (`compute_recommendation()`, `robustness_analysis()`, `run_scenario()`, `pareto_front()`, `catalogue_data()`): keep their arguments |
| The CSS/JS strings in `india_map_assets()` (`R/map.R`) and `REPORT_CSS` (`R/report.R`) | The rest of `R/map.R` and `R/report.R`, and the strings in section 4 that the R checks count |

## 3. Livability Robustness Engine (`R/robustness.R`)

`robustness_analysis(X, answers, weights, preference, n, seed, p_change, concentration, exact, data, flags)` re-runs the ranking under changed preferences and summarises how often each State/UT stays near the top.

- Every simulated ranking uses the project's own `run_ahp()` and `topsis()`; ties share the best rank, as in the baseline.
- `preference = "ratings"`: each of the seven answers moves one point up or down with probability `p_change` (half each way, clipped to 1-5) and the weights are recomputed by AHP. Monte Carlo with a seed, or `exact = TRUE` to enumerate all 3^7 = 2,187 shift combinations weighted by probability.
- `preference = "dirichlet"`: weights drawn from Dirichlet(`concentration` x baseline weights), the same model as `analysis/10_sensitivity.R`; zero weights stay zero.
- `preference = "none"`: baseline weights in every simulation (reproduces the baseline ranking exactly).
- `data`: optional `data_uncertainty(lower, upper, source)` bounds on criterion scores, sampled uniformly. They must name their source. The repository contains no evidence for such bounds (no sources, years or error estimates; see `docs/DATA_AUDIT.md`), so no default is provided.
- States/UTs with a missing score are reported in `not_comparable`, never imputed. `flags` carries data notes (for example low confidence) into the summary without changing any number.
- Output: per State/UT the baseline rank and closeness, shares (and counts) at rank 1 and in the top 3, 5 and 10, the median, 5/25/75/95 % quantiles and mean simulated rank; the stability of the baseline #1; competitive alternatives (top-3 share of at least 25 % by default); and, when weights vary, the Spearman correlation between each criterion's simulated weight and the baseline #1's simulated rank (an association within the simulations, not a cause).

The shares are conditional on the chosen assumptions. They are not probabilities that a place is objectively the best place to live.

## 4. Strings the R checks depend on

Several checks in `analysis/` count exact strings in generated HTML. Keep them when changing the markup:

| Where | String or pattern | Checked by |
|---|---|---|
| map | `data-state=` appears exactly 43 times (36 outlines + 7 markers) | `analysis/07_map.R` |
| map | `data-badge-state=` exactly 3 times (top-3 badges) | `analysis/07_map.R` |
| map | class string starts `ilm-state ilm-low` for the 4 low-confidence outlines | `analysis/07_map.R` |
| map | colours from `closeness_colours()` are uppercase `#RRGGBB` and get darker as closeness rises | `analysis/07_map.R` |
| map | excluded States/UTs use `fill="#E5E7EB"` and the text "Excluded from this ranking" exactly 7 times | `analysis/08_app_check.R` |
| report | each ranking row is written as `<tr>` followed by `<td>` (no attributes on `<tr>`); other tables in the report give their rows a class | `analysis/09_report_check.R` |
| report | `<title>My Livability Report</title>`, headings `1. <State>`, the text "low data confidence", "Not ranked (left out by choice)", "Equal weights (questions skipped)" | `analysis/09_report_check.R` |
| text | `length(METHOD_STEPS) == 8`, `length(LIMITATIONS) == 7`, the Manipur value 1.32 | `analysis/09_report_check.R` |
| app | `app/app.R` sources and returns a `shiny.appobj`; `www/style.css` and `www/img/logo.svg` exist | `analysis/08_app_check.R` |

## 5. Checks without R

`tools/` holds checks that need only Python 3.9+ (standard library) or Node.js; see [TESTING.md](TESTING.md). `data/indicator_metadata.csv` is the machine-readable data dictionary; `tools/check_repo.py` keeps it consistent with `R/common.R` and `analysis/01_audit.R`. `tools/mcda.py` is a small Python version of AHP and TOPSIS used to re-check committed results and for the audit's what-if analysis; the app never uses it. `tests/browser/` holds a smoke test that drives the running app.
