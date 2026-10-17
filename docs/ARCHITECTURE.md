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
R/app_text.R    METHOD_STEPS, LIMITATIONS, DATA_NOTES, STABILITY_NOTES, BEFORE_YOU_MOVE (shared text)
R/data_quality.R build_criterion_scores(), coverage_table(), coverage_summary(),
                limited_coverage_states(), indicator_detail(), CRITERION_INFO
R/view_models.R recommendation_view(), comparison_view(), place_detail(), catalogue_view(),
                stability_view(), tradeoff_sentences()   (interface contracts, no HTML)
R/sensitivity.R dirichlet_weights(), rank_stability(), rank_without() (analysis/10_sensitivity.R)
R/robustness.R  robustness_analysis()     (Livability Robustness Engine)
R/pareto.R      pareto_front(), explain_pareto()
R/catalogue.R   catalogue_data(), state_data_notes(), load_indicator_metadata()
R/news.R        parse_rss(), select_news(), fetch_news(), ensure_news_refresh(), cache helpers
R/scenario.R    run_scenario(), scenario_view(), stability_check()
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

Every tab reads the same `rec()` (AHP + TOPSIS for the current answers and the low-confidence switch), so ranks and scores agree across tabs and the report. `LOW_CONF` comes from `limited_coverage_states()` (`R/data_quality.R`), not from the stored flag.

Functional changes made to `app/app.R` in the product review (markup and classes kept; see [PRODUCT_DECISIONS.md](PRODUCT_DECISIONS.md)): new modules sourced; question titles use `QUESTIONS$Label`; verdict names are positions (`Top third` etc., same CSS classes); top cards show "Relative match" to 2 decimals, stability counts and coverage notes; the Top 3 tab shows the close-call/alternatives sentence and the alternative top 3 without limited-coverage places; editing answers keeps the previous answers, `mode_note` says what changed and offers `restore_previous`; the catalogue detail uses `place_detail()` (respects the switch, shows raw values); the report receives `d` and `meta`; texts no longer mention the eigenvector, TOPSIS closeness or "low confidence" in the main views; the News message explains that it is off by default. The robustness settings map "How sure are you?" to `p_change` 0.10 / 0.25 / 0.50 (ratings) or concentration 200 / 50 / 15 (weights); these are assumptions the user chooses, documented in the tab.

### Handover: presentation versus calculations

| Safe to redesign freely (presentation) | Do not change without the calculation tests (logic) |
|---|---|
| `app/www/style.css`, `app/www/table.js`, `app/www/img/` | `R/common.R`, `R/ahp.R`, `R/topsis.R`, `R/app_logic.R`, `R/explain.R`, `R/similarity.R`, `R/compare.R`, `R/sensitivity.R` |
| The UI functions in `app/app.R` (`welcome_screen()`, `question_screen()`, `top_card()`, `ranking_table()`, `weights_panel()`, the `*_panel()` functions, `results_screen()`, `ui`) and the markup inside the `render*` blocks | `R/robustness.R`, `R/pareto.R`, `R/scenario.R`, `R/catalogue.R`, `R/news.R`, `R/app_text.R` (texts quoted by checks), `R/data_quality.R`, `R/view_models.R` |
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

Several checks count exact strings in generated HTML or text. Keep them when changing the markup, or update the check in the same change:

| Where | String or pattern | Checked by |
|---|---|---|
| map | `data-state=` appears exactly 43 times (36 outlines + 7 markers) | `analysis/07_map.R` |
| map | `data-badge-state=` exactly 3 times (top-3 badges) | `analysis/07_map.R` |
| map | class string starts `ilm-state ilm-low` for the 4 limited-coverage outlines | `analysis/07_map.R` |
| map | colours from `closeness_colours()` are uppercase `#RRGGBB`, on a fixed 0-1 scale, and get darker as closeness rises | `analysis/07_map.R`, `test-view-models.R` |
| map | excluded States/UTs use `fill="#E5E7EB"` and the text "Excluded from this ranking" exactly 7 times | `analysis/08_app_check.R` |
| report | each ranking row is written as `<tr>` followed by `<td>` (no attributes on `<tr>`); other tables in the report give their rows a class | `analysis/09_report_check.R`, `test-catalogue-report.R` |
| report | `<title>My Livability Report</title>`, headings `1. <State>`, "limited data coverage", "Not ranked (left out by choice)", "Equal weights (questions skipped)", "Data notes for the places in this report", "Before you move, check", "Evidence: " | `analysis/09_report_check.R`, `test-catalogue-report.R` |
| text | `length(METHOD_STEPS) == 8`, `length(LIMITATIONS) == 8`, the Manipur value 1.32, the four limited-coverage names in `LIMITATIONS` | `analysis/09_report_check.R`, `tools/check_repo.py` |
| explanations | verdicts `Top third` / `Middle third` / `Bottom third`; "furthest short for you:"; weights message contains "times as much" | `analysis/08_app_check.R`, tests |
| app | `app/app.R` sources and returns a `shiny.appobj`; `www/style.css` and `www/img/logo.svg` exist | `analysis/08_app_check.R` |

## 5. View-model contracts

`R/view_models.R` (plus `scenario_view()` and `stability_check()` in `R/scenario.R`) is the interface for the redesigned UI. Every function is Shiny-free, returns plain data (no HTML), is deterministic, and is tested in `tests/testthat/test-view-models.R`. All of them work on the places in the current ranking (`rec$X`), so the shortlist, comparison, details, stability and report agree on what is included. Setup used in the examples:

```r
d    <- load_livability_data(); META <- load_indicator_metadata()
X    <- criteria_matrix(d);     LOW  <- limited_coverage_states(d, META)
ans  <- c(Education = 5, Safety = 5, Health = 5, Housing = 3, Economy = 3, Environment = 3, Connectivity = 2)
```

| Function | Inputs | Returns (fields) | Errors | Built on |
|---|---|---|---|---|
| `recommendation_view(ans, X, d, META, LOW, exclude_low = FALSE, k = 3)` | 7 named 1-5 answers | `rec`; `weights` (Criterion, Label, Rating, Weight, Percent, Sentence); `weights_note`; `shortlist` (Rank, State, Type, Relative_Match, Limited_Coverage, Coverage_Note, Stayed_Top_k, N_Variants); `reasons[[State]]` (Criterion, Label, Weight, Score, Position, Verdict, Support, Sentence, Evidence); `compromise[[State]]`; `stability`; `alternative_without_limited`; `excluded`; `n_ranked`; `notes` | incomplete or non 1-5 answers | `compute_recommendation()`, `priority_breakdown()`, `shortfall_sentence()`, `stability_check()`, `coverage_summary()` |
| `comparison_view(rec, states, d, META, LOW, similar_within = 0.05)` | 1-3 place names | `places` (State, Type, In_Ranking, Rank, Relative_Match, Limited_Coverage, Coverage_Note); `criteria` long (Criterion, Label, Weight, State, Score, Position, Support), ordered by weight; `indicators` long (State, Criterion, Indicator, Label, Raw, Unit_Display, Value_Text, Status, Provenance); `tradeoffs`; `notes` | 0 or more than 3 places, unknown names | `pareto_front()` (pairwise), `weights_table()`, `indicator_detail()` |
| `scenario_view(ans, changes, X, LOW, exclude_low, k = 3)` | previous answers; one or more new ratings, or all seven | `changed` (Criterion, From, To, Weight_Before, Weight_After); `top_before`; `top_after`; `entered`; `left`; `new_top`; `moves` (State, Before, After, Change); `sentences`; `scenario` | invalid ratings or names | `run_scenario()`, `rank_change()` |
| `stability_check(ans, X, LOW, exclude_low, k = 3)` | answers | `n_variants`; `top1`; `top1_kept`; `shortlist` (State, Rank, Stayed_Top_k, N_Variants); `close_call`; `also_consider` (State, Rank, Entered_Top_k); `variants`; `sentences`; `notes` | incomplete answers | `compute_recommendation()` |
| `stability_view(ans, X, LOW, exclude_low, k = 3, detail = FALSE)` | answers | `check` (as above); `detail` = NULL or `summary`, `competitive`, `assumption`, `interpretation` (exact enumeration, no seed) | as above | `robustness_analysis(exact = TRUE)` |
| `place_detail(rec, d, META, state, LOW)` | one place name | `state`, `type`, `region`, `in_ranking`, `rank`, `n_ranked`, `relative_match`, `limited_coverage`, `coverage_note`; `criteria` (Criterion, Label, Score, Position, Position_Group, Available, Total, Support); `indicators` (evidence with Raw, Unit_Display, Value_Text, Status, Provenance, Evidence); `similar`; `similar_note`; `standing` | unknown name | `pareto_front()`, `similar_states()`, `indicator_detail()` |
| `catalogue_view(rec, d, META)` | the current recommendation | `catalogue_data()` columns plus Limited_Coverage, Coverage_Note, Imputed | — | `catalogue_data()`, `coverage_summary()` |
| `build_report_html(rec, X, LOW, source, state_type, shortlist, scenario, d, META)` | as before (the `pareto` argument was removed) | one HTML string | — | the functions above |

Examples:

```r
v <- recommendation_view(ans, X, d, META, LOW)
v$shortlist$State                     # "Himachal Pradesh" "Chandigarh" "Punjab"
v$stability$sentences[1]              # "Your #1, Himachal Pradesh, stayed first in 8 of 11 small changes to your answers."
v$alternative_without_limited         # "Himachal Pradesh" "Punjab" "Maharashtra"
comparison_view(v$rec, c("Himachal Pradesh", "Kerala"), d, META, LOW)$tradeoffs[1]
# "There is no overall winner: Himachal Pradesh is stronger on Safety, Housing; Kerala is stronger on ..."
scenario_view(ans, c(Safety = 2, Environment = 5), X, LOW)$sentences
place_detail(v$rec, d, META, "Lakshadweep", LOW)$criteria   # Connectivity: Support "imputed"
```

Field meanings that must not be presented differently:

- `Score` is a **relative** 0-1 score; `Raw`/`Value_Text` is the original measurement. Do not label one as the other.
- `Relative_Match` is relative to the places ranked; show `MATCH_NOTE` (or equivalent wording) near it.
- `Stayed_Top_k`/`N_Variants` and robustness shares are counts or shares of tested answer variations, never probabilities.
- `Support` (`full`/`partial`/`thin`/`imputed`) and `Provenance` (`source not traced`/`source not recorded`) must stay visible wherever the value is shown.
- `similar` does not use the user's priorities; keep `similar_note` with it.

### Handover to the UI/UX teammate

- Call the functions above from the server; do not read `rec()` internals or compute scores in the UI. Their field names are the contract; changes to them go through the tests.
- Presentation is yours: layout, tabs, styling, wording of headings. Keep the meaning of the fields above and the strings in section 4 (or update the named checks together).
- The current `app/app.R` is the original interface with targeted functional edits (section 2). If your redesign replaces it, port these behaviours: `LOW_CONF` from `limited_coverage_states()`, the stability and alternative-top-3 notes, the what-changed/restore flow, `place_detail()` for details, `d`/`meta` passed to the report, News off by default.
- `tests/browser/smoke.js` checks the original markup (class names such as `.tc-name`, `.match-label`, `.cr-box`) and asserts that redesigned elements are absent; replace it with checks on stable hooks when your redesign lands.

## 6. Checks without R

`tools/` holds checks that need only Python 3.9+ (standard library) or Node.js; see [TESTING.md](TESTING.md). `data/indicator_metadata.csv` is the machine-readable data dictionary; `tools/check_repo.py` keeps it consistent with `R/common.R` and `analysis/01_audit.R`. `tools/mcda.py` is a small Python version of AHP and TOPSIS used to re-check committed results and for the audit's what-if analysis; the app never uses it. `tests/browser/` holds a smoke test that drives the running app.
