# Product and data decisions

Decisions taken in the product review (17 October 2026), what they changed, and what is still unresolved. Code references are to `R/`; the data facts behind them are in [DATA_AUDIT.md](DATA_AUDIT.md).

**No ranking changed.** Every decision below either documents, flags or explains the existing scores. The AHP and TOPSIS calculations, the stored criterion scores and the persona results in the README are unchanged.

## 1. Who the product is for and what it may claim

The app helps ordinary people in India find States/UTs that **may** suit their priorities, see why, compare them and see how stable the result is. It compares State/UT-level indicators only. It must not claim to know rent, salaries, job openings, the cost of living, public transport, distance to hospitals, personal safety, language fit or daily life in a particular city. Words such as best, safest, affordable, reliable or "high confidence" are not used.

## 2. Data and scoring decisions

| # | Issue | Decision | Effect on rankings | Where |
|---|---|---|---|---|
| 1 | The script that built the stored criterion scores was not in the repository | `build_criterion_scores()` rebuilds them from the 17 raw indicators with the rule found by the audit; a test checks all 252 cells (largest difference 1.5e-4, i.e. 4-decimal rounding) | None: the app still ranks with the stored scores | `R/data_quality.R`, `tests/testthat/test-data-quality.R` |
| 2 | Lakshadweep's Connectivity score is imputed (median of the other 35) | Kept, so rankings do not change, but flagged everywhere as **estimated, not measured** (coverage level `imputed`); the pipeline reproduces it with an explicit median rule | None | `coverage_table()`, `coverage_summary()` |
| 3 | Missing indicators are skipped, so a score can rest on 1 of 3 indicators | Not penalised (that would change rankings without source evidence), but every criterion score now has a coverage level: `full`, `partial` (at least half), `thin` (fewer than half) or `imputed` (none). Shown with scores, in details, comparisons and the report | None | `coverage_table()` |
| 4 | "Low confidence" was a single threshold (3+ missing) | Replaced by a coverage rule: **limited data coverage** = 3+ of 17 missing, OR any `thin` or `imputed` criterion. On the current data it flags the same four places (A&N Islands, Chandigarh, Ladakh, Lakshadweep); the five places with 1-2 missing indicators get per-criterion notes. They stay visible and ranked by default; the existing switch leaves them out reversibly, and the shortlist shows the alternative top 3 without them | None (same set) | `limited_coverage_states()`, `app/app.R` (`LOW_CONF`) |
| 5 | Values above 100 (clean cooking fuel 22 places, immunisation 7) | Not treated as errors (the definitions are unknown). Capping at 100 kept, and each capped cell is marked `capped at 100`; raw values stay visible | None | `scale_indicator()`, `indicator_detail()` |
| 6 | 5th/95th percentile capping (e.g. Delhi PM2.5) | Kept (it reproduces the stored scores) and disclosed; each capped cell is marked | None | `DATA_NOTES`, cell status |
| 7 | Untraceable indicators (Education score, Crime rate, Price stability, EVs per lakh) | Kept in the ranking but labelled **source not traced** wherever their values are shown (`provenance_status` in the metadata). The other 13 are labelled **source not recorded**. No source, year or unit was invented | None | `data/indicator_metadata.csv` |
| 8 | Inflation direction (lower always better) and Manipur's 1.32 | Kept and disclosed as a method choice; removal needs source evidence and an owner decision (research list, item 3) | None | `LIMITATIONS`, metadata `interpretation_limits` |
| 9 | Criterion names suggest more than is measured | Internal names unchanged (they are column names); public labels and descriptions say what is measured and what is not, e.g. "Housing (basic household amenities): not rent or house prices" | None | `CRITERION_INFO`, `QUESTIONS$Label`, `QUESTIONS$Hint` |
| 10 | 0-1 scores could be mistaken for measurements | Scores are called "relative score"; raw values with units (only where the metadata establishes them, otherwise "unit not documented") are shown next to them | None | `indicator_evidence()` in `R/view_models.R` |
| 11 | The prompt for this work listed "immunisation per lakh" as untraceable | No such indicator exists. Immunisation coverage is a percentage with an unrecorded source; the fourth untraceable indicator is EVs per lakh | — | — |

## 3. Recommendation and communication decisions

| # | Decision | Why | Where |
|---|---|---|---|
| 1 | AHP and TOPSIS unchanged | Mathematically sound and tested; changing them would change every result | `R/ahp.R`, `R/topsis.R` |
| 2 | The consistency ratio is no longer shown as reassurance | With 1-5 answers it always passes (tested), so it says nothing. The weights message now explains magnification: "your top criterion counts about N times as much as your lowest" | `consistency_message()` |
| 3 | "Strong match / Moderate match / Falls short" (score thresholds 0.70/0.40) replaced by **Top third / Middle third / Bottom third** of the places ranked | Scores are relative; a fixed threshold implied an absolute quality the data cannot support | `priority_breakdown()`, `position_group()` |
| 4 | Match score shown as "relative match" to 2 decimals with a note that it is relative and changes if places are left out | Three decimals implied precision; the score is not a livability percentage | `MATCH_NOTE`, `app/app.R` top cards, report |
| 5 | **Stability check**: each answer moved one point up and down, one at a time (up to 14 variants), counted, e.g. "stayed in your top 3 in 13 of 14" | Deterministic, explainable, and a count rather than a probability; identifies close calls and alternatives worth comparing | `stability_check()` in `R/scenario.R` |
| 6 | The robustness engine stays, run with exact enumeration (no simulation count, no seed) in the Stability tab; the simple counts come first and the table is under "Details for advanced users" | Shares are "shares of tested answer variations", never probabilities | `R/robustness.R`, `robustness_view()` |
| 7 | Changing answers is the what-if workflow: the Change my priorities tab (several ratings at once) and the questionnaire share one baseline, the original answers; the app shows what changed and offers "Go back to my original answers". The single-rating What if tab was removed | Same engine and exclusion setting; no separate scenario tool | `preferences_view()`, `scenario_view()`, `app/app.R` |
| 8 | Pareto analysis turned into plain trade-off sentences ("There is no overall winner: A is stronger on X; B on Y"; "On every criterion, A scores at least as well as B") with the most important difference for the user's weights | A 7-criterion frontier with 25 of 36 places says little; no "Pareto" wording outside the advanced Trade-offs tab | `tradeoff_sentences()`, `comparison_view()`, `place_detail()` |
| 9 | Similar places are found among the places ranked (the app already did this; the report used all 36) and always labelled "does not use your priorities" | Consistency with the exclusion switch | `R/report.R`, `place_detail()` |
| 10 | Map colours use a fixed 0-1 scale | The stretched scale made small differences look large; the legend states the scale and the actual range | `closeness_colours()` |
| 11 | The report summarises what the user sees: reasons with indicator evidence, stability, data notes for the places it names, comparison trade-offs, limits and a "before you move" checklist. The 7-criterion Pareto list was removed | Report and app must agree | `build_report_html()` |
| 12 | News is **off by default** (`LIVABILITY_NEWS=on` to enable) | Headlines are not about particular places, are not used in the rankings, and the feature started a background process and wrote a cache file on every app start. Kept in code (tested) rather than deleted, so the teammate's design can decide whether the tab stays | `R/news.R` |

## 4. Intended user journey

1. **Introduction**: what the tool can and cannot tell you; start, choose an example (preset) or "no strong preferences" (equal weights).
2. **Your priorities**: seven 1-5 questions whose titles and hints say what is actually measured.
3. **Your shortlist**: top 3 with relative match, reasons by position and evidence, biggest gap, stability counts, close-call notice, alternatives worth comparing, data-coverage notes, the alternative top 3 without limited-coverage places; map and full list as other views of the same ranking.
4. **Place details**: relative scores with positions, coverage, raw indicator values with units, status and provenance, similar places (unweighted).
5. **Compare** up to three: scores and positions by the user's weights, plain trade-offs, data notes.
6. **Change preferences**: edit answers and see what changed; go back.
7. **About the data and limits**, and **Save my report**.
8. **Advanced** (optional): full ranking, detailed robustness, two-criterion trade-off plot.

The functions behind each step are listed in [ARCHITECTURE.md](ARCHITECTURE.md#5-view-model-contracts). How the screens look and are arranged is the UI/UX teammate's decision.

**Status:** this journey is implemented as tested functions and connected to the current tab-based interface: Top 3 cards; What if? (several ratings, original and new top 3 side by side, back to original); Compare (bars per criterion); Stability (one question, visual answer). The scatter chart and the robustness tables are no longer shown. The visual presentation is still the original design; restyling and rearranging it is the redesign's job. The table in [ARCHITECTURE.md](ARCHITECTURE.md#handover-to-the-uiux-teammate) lists what is visible now and what is not.

## 5. Unresolved research (needs sources or an owner decision)

These cannot be settled from the repository. Nothing was guessed.

1. **Sources and reporting years for all 17 indicators** (publisher, table, URL, year). Fill `source`, `reporting_period` and `provenance_status` in `data/indicator_metadata.csv`, then re-run `python3 tools/audit_data.py`.
2. **Definitions** of immunisation and clean-cooking-fuel coverage (why values exceed 100) and the units of Crime rate, Health-worker density and PM2.5.
3. **Inflation**: verify Manipur's 1.32 and decide the direction (lower is better vs closeness to a target). Removing it changes every ranking and needs the persona expectations, README results and `outputs/` updated together.
4. **Untraceable indicators**: if Education score, Crime rate, Price stability or EVs per lakh cannot be sourced, decide whether to drop them. Dropping EVs leaves Connectivity with no data for six UTs (Villages 3G/4G is missing for them).
5. **Missing-data rule**: whether partial scores should be penalised (the audit's section 11 shows this decides the Retiree top 3), and whether Lakshadweep's imputed Connectivity should be excluded from its ranking.
6. **Outliers and possible copy errors** listed in DATA_AUDIT.md (Delhi crime and EVs, Meghalaya institutional deliveries, Arunachal kachha houses, identical inflation and PM2.5 pairs).
7. **Map outline licence** (udit-001/india-maps-data): confirm before redistributing the map.
