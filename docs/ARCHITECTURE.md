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
                stability_view(), robustness_view(), tradeoff_view(), default_tradeoff_axes(),
                preferences_view(), about_view(), tradeoff_sentences(), format_share()
                (interface contracts, no HTML)
R/sensitivity.R dirichlet_weights(), rank_stability(), rank_without() (analysis/10_sensitivity.R)
R/robustness.R  robustness_analysis()     (Livability Robustness Engine)
R/pareto.R      pareto_front(), explain_pareto()
R/catalogue.R   catalogue_data(), state_data_notes(), load_indicator_metadata()
R/news.R        parse_rss(), select_news(), fetch_news(), ensure_news_refresh(), cache helpers
R/scenario.R    run_scenario(), scenario_view(), stability_check()
R/atlas.R       render_tradeoff_plot(), atlas_assets()  (trade-off scatter plot, drawing only)
        |
        +--> app/app.R            Shiny interface (original design)
        +--> analysis/01-11       numbered scripts: checks, tables, figures, maps
        +--> tests/testthat/      automated R tests
```

The rule that keeps this maintainable: **all calculations live in `R/`**. `app/app.R` only decides which screen to show and reacts to input; the analysis scripts and the tests call the same functions, so the app and the published results cannot disagree.

## 2. The Shiny app

The interface is the original one from the repository (commit `100fc77`): welcome screen, seven flashcard questions, results with tabs. Styles are in `app/www/style.css`; `app/www/table.js` provides search and sorting for the ranking table. Interface work is being taken over by another team member.

Result tabs: Top 3, What if?, Compare, Stability, Map, What matters to you, Full ranking, States & UTs, How it works, About and limits (News only when `LIVABILITY_NEWS=on`).

Changes to the original parts of `app/app.R`, none of which change how they look:

- The project logo (`app/www/img/logo.svg`) replaces the emoji on the welcome screen and is the browser-tab icon. The title is unchanged.
- Each answer button sends the question it belongs to (`{step, value}`); the server ignores an answer from a screen that has already been replaced (a double click used to answer the next question too) and accepts only whole numbers 1-5.
- `input$map_selected` is accepted only if it is the name of a State/UT.
- The report download also passes the comparison, the what-if scenario (if its rating differs) and the Pareto set to `build_report_html()`.

### Added tabs: where the calculations come from

| Tab | Inputs | Server output | Calculation (R/) |
|---|---|---|---|
| Compare | `tc_x`, `tc_y`, `cmp_places`, `cmp_click` (chart click), `cmp_top3`, `cmp_clear` | `output$tc_chart`, `output$tc_cards`, `output$compare_more` | `tradeoff_chart_data()` (uses `tradeoff_view()` / `pareto_front()`), `toggle_place()`, `resolve_axes()`, `comparison_bars_view()`; drawn by `render_tradeoff_plot()` |
| Robustness | `rb_method`, `rb_level`, `rb_n`, `rb_seed`, `rb_run`; `rb_csv` download | `output$robustness`, `output$rb_csv` | `robustness_analysis()`, `dirichlet_weight_sd()` |
| What if | `wi_crit`, `wi_value` | `output$whatif` | `run_scenario()` (uses `compute_recommendation()`, `rank_change()`) |
| States & UTs | `cat_search`, `cat_pick`, `cmp_pick` | `output$catalogue`, `output$cat_detail`, `output$compare` | `catalogue_data()`, `state_data_notes()`, `explain_pareto()` |
| News | none | `output$news` | `read_news_cache()`, `ensure_news_refresh()` (one background `callr` process for all sessions, polled for up to 25 s; falls back to a cache up to 48 h old or an "unavailable" message) |

Every tab reads the same `rec()` (AHP + TOPSIS for the current answers and the low-confidence switch), so ranks and scores agree across tabs and the report. `LOW_CONF` comes from `limited_coverage_states()` (`R/data_quality.R`), not from the stored flag.

Functional changes made to `app/app.R` in the product review (markup and classes kept; see [PRODUCT_DECISIONS.md](PRODUCT_DECISIONS.md)): new modules sourced; question titles use `QUESTIONS$Label`; verdict names are positions (`Top third` etc., same CSS classes); top cards show "Relative match" to 2 decimals, stability counts and coverage notes; the Top 3 tab shows the close-call/alternatives sentence and the alternative top 3 without limited-coverage places; editing answers keeps the previous answers, `mode_note` says what changed and offers `restore_previous`; the catalogue detail uses `place_detail()` (respects the switch, shows raw values); the report receives `d` and `meta`; texts no longer mention the eigenvector, TOPSIS closeness or "low confidence" in the main views; the News message explains that it is off by default.

Fifth pass (trade-off explorer): **Compare** is now one trade-off explorer. Two axis selectors (`tc_x`, `tc_y`, starting on the user's two most important criteria; choosing the other axis's criterion swaps them, `resolve_axes()`), a searchable selector of up to three places (`cmp_places`, starting on the top 3; "Use my top 3", "Clear"), the scatter plot (`render_tradeoff_plot()` on `tradeoff_chart_data()`: one point per ranked place at its relative scores, fixed 0-1 scales, "→ better" axis titles, "Better on both ↗", small soft points, your top 3 in purple, selected places in amber with non-overlapping direct labels, dashed outlines for limited or estimated data, tooltips, a legend; missing scores listed, never drawn at zero), a card per selected place (rank, relative match, both scores with positions, one observation, a warning when an axis score is incomplete or the place has limited coverage) and, collapsed, all seven criteria and the original indicator values. Clicking a point (or Enter on it) toggles it in `cmp_places` (`toggle_place()`; the map script sends points with `data-click-input` to that input instead of the map selection), so the chart, the search box and the cards always show the same selection. The previous all-criteria bar list moved into the collapsed details. `render_atlas()` was replaced by `render_tradeoff_plot()`.

Fourth pass (user-experience fix; targeted changes to the current UI, existing classes plus one new stylesheet `app/www/features.css` that only uses the variables of `style.css`): **Top 3** cards show rank (#1, no medals), a coverage badge only where data are missing (`coverage_badge()`), the relative match to 2 decimals, "Why it matches" (position on the three top priorities, from `place_reasons()`), one trade-off line and the indicator evidence behind "Show the data"; one stability line (label from `stability_summary_view()`) and the alternative top 3; the relative-match caveat appears once. **What if?** (second tab) shows the seven ratings (`preferences_view()`), "Show results with these priorities" and "Back to my original answers"; `whatif_view()` draws the original and new top 3 side by side with ▲/▼/new markers and explains the change from the weights and scores; the header says when a what-if is shown. **Compare** lets the user pick two or three places (default: the top 3, "Use my top 3" resets) and shows one bar per place for every criterion, ordered by weight, with clear leaders, "about the same" near-ties, coverage flags and up to two observations (`comparison_bars_view()`); original indicator values are behind "See the original indicator values". **Stability** keeps one question ("How sure are you of your answers?") and shows a label, a headline, a gauge, one bar per main place and the caveat once, with a short "What does this mean?" (`stability_summary_view()`, exact engine, no seed); the simulation table, CSV and "advanced users" section were removed. The two-criterion scatter chart (`render_atlas()`, `tradeoff_view()`) is no longer in the interface; the code and its tests remain. The compare box under States & UTs was removed (Compare replaces it). **What matters to you** uses `weights_view()`; How it works and About and limits use `about_view()`; the full ranking dropped D+/D-; the map detail shows the same card as the Top 3; the report moved the full ranking to an appendix. The News tab is hidden unless News is switched on.

Third pass (authorised targeted changes to the existing controls, still using only the existing CSS classes): the Top 3 cards are built from `recommendation_view()` (reasons with indicator evidence) and the tab explains the main difference between #1 and #2 (`tradeoff_sentences()`); **Trade-offs** became **Compare & trade-offs**, which leads with a plain comparison of the top 3 (`comparison_view()`: trade-off sentences, positions by weight, the original indicator values behind the top priorities) and keeps the two-criterion chart under an "Advanced" `<details>`; **Robustness** became **Stability**: the method, simulation-count, seed and run controls were removed, it leads with `stability_check()` and the exact (`exact = TRUE`, no seed) `robustness_analysis()` summary, with the table and CSV under "Details for advanced users"; **What if** became **Change my priorities**: seven rating selectors (`preferences_view()`) and "Update my results" replace the current answers everywhere, the original answers are kept as `state$baseline` (also when editing through the questionnaire), `scenario_view()` shows what changed and "Go back to my original answers" restores them; the report's `changes` section uses the same baseline (the separate what-if scenario is no longer passed); map hover cards and the map detail show coverage notes (`render_india_map(notes = )`); the welcome text no longer says "fits your life best".

Second pass (same rules: content and data sources only, no layout or CSS change): the Trade-offs tab starts on the user's two most important criteria and leads with `tradeoff_view()` sentences before the chart (the "Pareto-efficient" wording and the 7-criteria note are gone); the Robustness tab's wording comes from `robustness_view()` and its data notes from `coverage_summary()`; the comparison under States & UTs adds `comparison_view()` trade-offs and data notes; the catalogue's last column is "Data coverage" (limited / partial / complete); the ranking table says "limited data coverage"; How it works lists what each criterion does not measure; About and limits adds `DATA_NOTES` and `BEFORE_YOU_MOVE`; the report includes what changed since the previous answers. The robustness settings map "How sure are you?" to `p_change` 0.10 / 0.25 / 0.50 (ratings) or concentration 200 / 50 / 15 (weights); these are assumptions the user chooses, documented in the tab.

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
| `robustness_view(rb, top_n = 10)` | a `robustness_analysis()` result | `top_state`, `top1_share`, `top3_share`, `n_text`, `headline`, `competitive`, `table` (State, Your_Rank, Match, Rank_1, Top_3, Top_5, Top_10, Simulated_Rank, Data_Note as display strings), `table_note`, `influence`, `assumption`, `interpretation`, `data_note`, `not_compared` | — | `format_share()` |
| `whatif_view(baseline, answers, X, LOW, exclude_low, k = 3)` | original and alternative answers | `unchanged`; `changed` (Criterion, From, To, Weight_Before, Weight_After); `before` (Rank, State, Status = stays/leaves); `after` (Rank, State, Was, Move = "▲ n"/"▼ n"/"same"/"new", New); `moves`; `entered`; `left`; `new_top`; `summary`; `explanation` (weight changes and the score difference that drove the main move) | ratings outside 1-5 or fewer than seven | `scenario_view()`, `run_scenario()` |
| `stability_summary_view(ans, X, d, META, LOW, exclude_low, p_change = 0.25, max_places = 5)` | answers, "how sure" level | `top_state`, `label` (Holds up >= 0.9 / Usually holds >= 0.5 / Close call), `label_class`, `top1_share`, `headline`, `sentence`, `places` (State, Rank, First_Share, Top3_Share, First_Label, Top3_Label, Badge), `caveat`, `explain`, `check` | as `stability_check()` | `stability_check()`, `robustness_analysis(exact = TRUE)`, `robustness_view()` |
| `comparison_bars_view(rec, states, d, META, LOW)` | 1-3 places (2-3 for observations) | `places`; `bars` (Criterion, Weight, Percent, State, Score, Position, Flag, Leads, Tie); `observations` (up to two); `badges`; `indicators`; `n_ranked`. Near-tie conventions: `NEAR_TIE_SCORE` = 0.05 per criterion, `NEAR_TIE_MATCH` = 0.02 overall | as `comparison_view()` | `comparison_view()` |
| `place_reasons(rec, d, META, state, k = 3)` | a ranked place | Criterion, Label, Weight, Score, Position, Verdict, Support, Sentence, Evidence | unknown place | `priority_breakdown()`, `indicator_evidence()` |
| `coverage_badge(d, META, state)` | a place | one short string, "" when complete | — | `coverage_summary()` |
| `tradeoff_chart_data(rec, x, y, d, META, selected, LOW)` | two different criteria; 0-3 selected places | `x`, `y`, `x_title`, `y_title`, `n_ranked`; `points` (State, Short, X, Y, Rank, Match, Top3, Selected, Frontier, Support_X, Support_Y, Estimated, Partial, Limited); `cards` (State, Rank, Match, Score_X, Score_Y, Pos_X, Pos_Y, Observation, Warning) in selection order; `selected`; `excluded_selected`; `not_plotted`; `note` | same or unknown criteria | `tradeoff_view()`, `coverage_table()`, `coverage_badge()` |
| `render_tradeoff_plot(cd, click_input = "cmp_click")` | a `tradeoff_chart_data()` result | an SVG chart with legend (htmltools tag); points carry `data-state`, `data-tip`, `data-click-input` | — | `R/atlas.R` |
| `toggle_place(current, clicked, valid, max_n = 3)` / `resolve_axes(x, y, previous)` | selection and click / axis choice | the new selection / an axis pair that always differs | — | — |
| `tradeoff_view(rec, x, y)` | two different criteria | `x`, `y`, `frontier` (State, Rank, Score_X, Score_Y: places no other place beats on both, in the user's rank order), `top_state`, `top_on_frontier`, `sentences` | same or unknown criteria | `pareto_front()` |
| `default_tradeoff_axes(w)` | weights | the two highest-weighted criteria | — | `by_weight()` |
| `preferences_view(ans)` | answers | Criterion, Label, Hint, Rating, Rating_Label, Weight, Percent (criterion order) | invalid answers | `run_ahp()` |
| `about_view()` | — | `scope`, `match_note`, `criteria` (`CRITERION_INFO`: Criterion, Label, Measures, Not_Measured), `method`, `limitations`, `data_notes`, `stability_notes`, `before_you_move` | — | `R/app_text.R` |
| `build_report_html(rec, X, LOW, source, state_type, shortlist, scenario, d, META, changes)` | as before (the `pareto` argument was removed); `changes` = a `scenario_view()` result for "what changed since my previous answers" | one HTML string | — | the functions above |

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
tradeoff_view(v$rec, "Education", "Safety")$sentences         # default axes for this user
preferences_view(ans)                                         # editor rows: current ratings and weights
rb <- robustness_analysis(v$rec$X, answers = ans, preference = "ratings", exact = TRUE, p_change = 0.25)
robustness_view(rb)$headline                                  # no seed or simulation count needed
```

Field meanings that must not be presented differently:

- `Score` is a **relative** 0-1 score; `Raw`/`Value_Text` is the original measurement. Do not label one as the other.
- `Relative_Match` is relative to the places ranked; show `MATCH_NOTE` (or equivalent wording) near it.
- `Stayed_Top_k`/`N_Variants` and robustness shares are counts or shares of tested answer variations, never probabilities.
- `Support` (`full`/`partial`/`thin`/`imputed`) and `Provenance` (`source not traced`/`source not recorded`) must stay visible wherever the value is shown.
- `similar` does not use the user's priorities; keep `similar_note` with it.

### Handover to the UI/UX teammate

**Status of each product change** (17 October 2026):

| Change | Calculation / contract | Visible in the current interface | Needs the redesign |
|---|---|---|---|
| Plain stability instead of simulation controls | `stability_summary_view()` | **Done in the current UI**: Stability tab with one question, label, gauge, bars, caveat once; no tables, seed or count | Visual styling |
| What-if, several changes, before/after | `preferences_view()`, `whatif_view()` | **Done in the current UI**: What if? tab (second), side-by-side original/new top 3 with movement, explanation, back to original; header banner; report section | Visual styling; e.g. an inline editor on the results |
| Two-criterion trade-off explorer | `tradeoff_chart_data()`, `render_tradeoff_plot()`, `toggle_place()`, `resolve_axes()` | **Done in the current UI**: Compare tab with axis selectors, scatter plot, chart/search selection kept in sync, cards; all criteria and raw values collapsed | Visual styling; keep `data-click-input` (or call `toggle_place()` from your own click handler) |
| Data coverage and limits clear | `coverage_summary()`, `indicator_evidence()`, `about_view()` | Coverage notes on top cards, details, comparison, catalogue, robustness; About and limits shows data notes and the checklist | Show `Support` with each score, `Provenance` and `Value_Text` with each indicator, and `MATCH_NOTE`/`SCOPE_NOTE` near results, in your own design. |
| Consistent scoring and exclusion | every view model uses `rec$X` | Verified by `analysis/12_server_check.R` (25 checks) | Keep passing one `rec` (from `compute_recommendation()` with the switch) to every view model. |
| News off by default | `news_enabled()` | Tab says it is off | Decide whether the tab stays at all. |
| Medals, D+/D- columns | — | **Done**: rank numbers instead of medals; Full ranking shows rank, relative match (2 decimals) and data coverage | — |

Rules for the redesign:

- Call the functions above from the server; do not read `rec()` internals or compute scores in the UI. Their field names are the contract; changes to them go through the tests.
- Presentation is yours: layout, tabs, styling, wording of headings. Keep the meaning of the fields above and the strings in section 4 (or update the named checks together).
- If your redesign replaces `app/app.R`, port these behaviours: `LOW_CONF` from `limited_coverage_states()`; Top 3 cards from `place_reasons()`, `coverage_badge()` and the `stability_summary_view()` label, plus the alternative top 3; the What if? flow (`preferences_view()`, `whatif_view()`, one baseline shared with the questionnaire, back to original, `changes` passed to the report); Compare (`tradeoff_chart_data()` + `render_tradeoff_plot()`, `toggle_place()`, `resolve_axes()`, cards, `comparison_bars_view()` collapsed); Stability (`stability_summary_view()`, one question); `place_detail()` for details; coverage notes in map hover cards; `about_view()` for methodology and limits; News off by default. `app/www/features.css` holds the layout rules these screens need; replace it with your own styles.
- Update the input/output names in `analysis/12_server_check.R` and replace `tests/browser/smoke.js` (it checks the original markup: `.tc-name`, `.match-label`, `.cr-box`, and asserts that redesigned elements are absent) with checks on stable hooks.

## 6. Checks without R

`tools/` holds checks that need only Python 3.9+ (standard library) or Node.js; see [TESTING.md](TESTING.md). `data/indicator_metadata.csv` is the machine-readable data dictionary; `tools/check_repo.py` keeps it consistent with `R/common.R` and `analysis/01_audit.R`. `tools/mcda.py` is a small Python version of AHP and TOPSIS used to re-check committed results and for the audit's what-if analysis; the app never uses it. `tests/browser/` holds a smoke test that drives the running app.
