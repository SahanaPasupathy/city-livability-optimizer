# ==============================================================================
# Server-level checks of the Shiny app (shiny::testServer, no browser needed)
# Run from the project root: source("analysis/12_server_check.R")
# Drives the CURRENT interface's inputs (input IDs of app/app.R) and checks what
# each screen shows: the Top 3, What if?, Compare and Stability screens, that
# no technical controls or "advanced users" sections appear, that detailed data
# stay available but secondary, and that every output uses the same scoring
# and the same included/excluded places. When the redesigned interface
# replaces app/app.R, update the input/output names here (the expectations stay
# valid). Writes nothing.
# ==============================================================================
suppressPackageStartupMessages(library(shiny))
cat("\n=== SERVER CHECKS (app/app.R) ===\n\n")
n_pass <- 0; n_fail <- 0
check <- function(label, ok) {
  ok <- isTRUE(ok)
  if (ok) n_pass <<- n_pass + 1 else n_fail <<- n_fail + 1
  cat(sprintf("[%s] %s\n", ifelse(ok, "PASS", "FAIL"), label))
}
html_of <- function(x) as.character(x$html)
text_of <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", html_of(x)))
old_news <- Sys.getenv("LIVABILITY_NEWS", NA)
Sys.unsetenv("LIVABILITY_NEWS")                       # check the default (off)

app <- shinyAppDir("app")       # also sources R/ into the global environment
d <- load_livability_data(); META <- load_indicator_metadata()
LOW_CONF <- limited_coverage_states(d, META); X <- criteria_matrix(d)
ORIGINAL <- setNames(c(4L, 3L, 5L, 2L, 4L, 1L, 3L), CRITERIA)

testServer(app, {
  session$setInputs(start = 1)
  for (v in ORIGINAL) session$setInputs(answer = list(step = state$step, value = v))
  check("Seven answers open the results", state$step == 8)
  top <- top_k(rec()$topsis, 3)$State
  check("Top 3 is Delhi, Kerala, Chandigarh", identical(top, c("Delhi", "Kerala", "Chandigarh")))

  # ---- Screen structure: plain tabs, no technical controls ----
  scr <- html_of(output$screen)
  tabs <- regmatches(scr, gregexpr('data-value="[^"]+"', scr))[[1]]
  tabs <- unique(sub('data-value="([^"]+)"', "\\1", tabs))
  check("Tabs: Top 3, What if?, Compare, Stability first; no Trade-offs/Robustness/News tab",
        identical(head(tabs, 4), c("Top 3", "What if?", "Compare", "Stability")) &&
          !any(c("Trade-offs", "Robustness", "Compare & trade-offs", "News") %in% tabs))
  check("No simulation-count, seed, method or run controls", !grepl("rb_n\"|rb_seed|rb_method|rb_run|Random seed|Simulations", scr))
  check("No 'advanced users' section and no Pareto chart in the screens",
        !grepl("advanced users|Advanced:", scr, ignore.case = TRUE) && !grepl("atlas_x|Pareto", scr))
  # Methodology (How it works) may name the methods; the result screens may not.
  primary <- paste(scr, html_of(output$top3))
  for (m in METHOD_STEPS) primary <- gsub(htmltools::htmlEscape(m), "", primary, fixed = TRUE)
  check("No technical terms in the result screens (methodology tab excluded)",
        !grepl("TOPSIS|eigenvector|D\\+|Pareto|closeness", primary, ignore.case = TRUE))

  # ---- Top 3 ----
  h <- html_of(output$top3)
  t3 <- text_of(output$top3)
  check("Top 3 cards show rank, relative match (2 decimals) and 'Why it matches'",
        grepl("#1", t3) && grepl("Relative match 0.79", t3) && grepl("Why it matches", t3) && !grepl("0\\.79[0-9]", t3))
  check("No medals on the cards", !grepl("&#129351;|gold|silver|bronze", h))
  check("Each card names its main trade-off", lengths(regmatches(t3, gregexpr("Trade-off:", t3))) == 3)
  check("Coverage warnings sit on the affected cards only",
        grepl("Limited data: Housing based on few indicators", t3) && !grepl("Kerala State &#9888;", h, fixed = TRUE))
  check("Stability label and the alternative top 3 without limited-coverage places are shown",
        grepl("Close call", t3) && grepl("Without places with limited data: Delhi, Kerala, Punjab", t3))
  check("Indicator evidence is behind 'Show the data', not in the default text",
        grepl("<summary>Show the data</summary>", h, fixed = TRUE))
  check("The caveat about relative match appears once", lengths(regmatches(t3, gregexpr("not a livability percentage", t3))) == 1)

  # ---- What if? ----
  check("What if starts on the original answers", grepl("viewing your original answers", text_of(output$whatif_result)))
  session$setInputs(pref_Education = "4", pref_Safety = "5", pref_Health = "5", pref_Housing = "2",
                    pref_Economy = "4", pref_Environment = "4", pref_Connectivity = "3", pref_apply = 1)
  NEW <- ORIGINAL; NEW[c("Safety", "Environment")] <- c(5L, 4L)
  check("Several ratings change in one scenario; the original answers are kept",
        identical(state$answers, NEW) && identical(unname(state$baseline), unname(ORIGINAL)))
  check("The scenario is ranked by the same engine and exclusion rule",
        identical(top_k(rec()$topsis, 3)$State, top_k(compute_recommendation(NEW, X, LOW_CONF, FALSE)$topsis, 3)$State))
  wr <- text_of(output$whatif_result)
  wv <- whatif_view(ORIGINAL, NEW, X, LOW_CONF, FALSE)
  check("What if shows original and new top 3 side by side with movement",
        grepl("Original top 3", wr) && grepl("With these priorities", wr) && grepl("leaves top 3", wr) &&
          grepl("new ▲ 4", wr) && identical(wv$after$State, top_k(rec()$topsis, 3)$State))
  check("What if explains the change with weights and scores",
        grepl("Safety now counts 29% of your result (was 8%)", wr, fixed = TRUE) && grepl("scores 0.97 there against 0.02", wr, fixed = TRUE))
  check("The results header says a what-if is being viewed", grepl("What if:", text_of(output$mode_note)))
  rep_edit <- paste(readLines(output$report, warn = FALSE), collapse = "\n")
  check("The report includes what changed compared with the original answers",
        grepl("What changed compared with my original answers", rep_edit, fixed = TRUE))
  # Change again: the baseline stays the original answers.
  session$setInputs(pref_Health = "3", pref_apply = 2)
  check("Changing ratings again keeps the same baseline", identical(unname(state$baseline), unname(ORIGINAL)) && state$answers[["Health"]] == 3L)
  # Invalid input is ignored.
  before <- state$answers
  session$setInputs(pref_Safety = "9", pref_apply = 3)
  check("An invalid rating is ignored", identical(state$answers, before))
  session$setInputs(pref_Safety = "5")
  session$setInputs(edit_answers = 1)
  for (v in c(4, 5, 5, 2, 4, 4, 3)) session$setInputs(answer = list(step = state$step, value = v))
  check("Editing through the questionnaire keeps the same original answers", identical(unname(state$baseline), unname(ORIGINAL)))
  session$setInputs(restore_previous = 1)
  check("Back to my original answers restores them and ends the what-if",
        identical(unname(state$answers), unname(ORIGINAL)) && is.null(state$baseline) &&
          grepl("viewing your original answers", text_of(output$whatif_result)))

  # ---- Stability ----
  st <- text_of(output$stability)
  ss <- stab_sum()
  check("Stability leads with a label, a headline and one sentence",
        grepl("Close call", st) && grepl("Delhi stays your #1 in 48% of the answer variations", st) && grepl("can change your #1", st))
  check("Stability shows a bar per main place (top 3 and alternatives)", nrow(ss$places) >= 3 &&
          all(top %in% ss$places$State) &&
          lengths(regmatches(html_of(output$stability), gregexpr("cmp-row", html_of(output$stability)))) == nrow(ss$places))
  check("Stability states the caveat once and has no tables",
        lengths(regmatches(st, gregexpr("not the chance that a place is best", st))) == 1 && !grepl("<table", html_of(output$stability)))
  check("Stability uses the single-change check and the exact engine", ss$check$n_variants == 12)
  session$setInputs(rb_level = "high")
  check("'How sure are you?' changes the result (Not sure: 38%)", grepl("in 38% of the answer variations", text_of(output$stability)))
  session$setInputs(rb_level = "medium")

  # ---- Compare: trade-off explorer ----
  check("Compare starts on your two most important criteria with your top 3 selected",
        grepl('<option value="Health" selected>Health</option>', scr, fixed = TRUE) &&
          grepl('<option value="Education" selected>Education</option>', scr, fixed = TRUE))
  check("Compare has one short explanation and no separate Trade-offs tab",
        grepl("Explore how States and UTs compare on two criteria", scr, fixed = TRUE) && !grepl(">Trade-offs<", scr, fixed = TRUE))
  session$setInputs(tc_x = "Health", tc_y = "Education", cmp_places = c("Delhi", "Kerala", "Chandigarh"))
  ch <- html_of(output$tc_chart)
  cd <- tc_data()
  check("Chart: one point per ranked place from the real scores", lengths(regmatches(ch, gregexpr('class="ilm-state tc-pt', ch, fixed = TRUE))) == 36 &&
          isTRUE(all.equal(cd$points$X, unname(X[cd$points$State, "Health"]))))
  check("Chart: selected places highlighted and labelled", lengths(regmatches(ch, gregexpr("is-selected", ch, fixed = TRUE))) == 3 &&
          grepl(">Delhi #1<", ch, fixed = TRUE))
  check("Chart: no Pareto wording, legend present", !grepl("Pareto", ch) && grepl("Your top 3", ch) && grepl("Limited or estimated data", ch))
  cards <- text_of(output$tc_cards)
  check("Cards agree with the ranking (rank, relative match, both scores)",
        grepl("Delhi #1 of 36 · relative match 0.79", cards, fixed = TRUE) && grepl("Health 0.89 (5th)", cards, fixed = TRUE) &&
          grepl("Education 0.73 (6th)", cards, fixed = TRUE))
  # Warnings are shown when an axis score is incomplete or the place has limited coverage:
  # Chandigarh (limited) gets one; Delhi's only gap (Housing) is not on these axes.
  check("Cards show a coverage warning only where it matters",
        grepl("Limited data: Housing based on few indicators", cards, fixed = TRUE) &&
          lengths(regmatches(html_of(output$tc_cards), gregexpr("badge-low", html_of(output$tc_cards)))) == 1)
  # Changing an axis keeps the selection.
  session$setInputs(tc_y = "Safety")
  check("Changing an axis updates the chart and keeps the selection",
        identical(tc_data()$y, "Safety") && identical(tc_data()$selected, c("Delhi", "Kerala", "Chandigarh")) &&
          grepl("Safety (relative score)", html_of(output$tc_chart), fixed = TRUE))
  # Identical axes: the chart says so, and the guard swaps them back.
  session$setInputs(tc_y = "Health")
  check("Identical axes are never drawn", is.null(tc_data()) && grepl("Choose two different criteria", text_of(output$tc_chart)) &&
          identical(resolve_axes("Health", "Health", c("Health", "Safety")), c("Safety", "Health")))
  session$setInputs(tc_y = "Education")
  # Clicking a point toggles it (the browser applies the update to cmp_places).
  sel <- toggle_place(input$cmp_places, "Goa", d$State)
  session$setInputs(cmp_click = "Goa", cmp_places = sel)
  check("Clicking a fourth place swaps out the earliest and updates chart and cards",
        identical(tc_data()$selected, c("Kerala", "Chandigarh", "Goa")) && grepl("Goa", text_of(output$tc_cards)) &&
          !grepl("Delhi #1 of", text_of(output$tc_cards), fixed = TRUE))
  sel <- toggle_place(input$cmp_places, "Goa", d$State)
  session$setInputs(cmp_click = "Goa", cmp_places = sel)
  check("Clicking a selected place deselects it", identical(tc_data()$selected, c("Kerala", "Chandigarh")))
  session$setInputs(cmp_places = character(0))
  check("With nothing selected the chart still shows every place and invites a choice",
        lengths(regmatches(html_of(output$tc_chart), gregexpr('class="ilm-state tc-pt', html_of(output$tc_chart), fixed = TRUE))) == 36 &&
          grepl("Click points or search above", text_of(output$tc_cards)) && is.null(output$compare_more))
  session$setInputs(cmp_places = c("Delhi", "Kerala", "Chandigarh"))
  more <- html_of(output$compare_more)
  check("All criteria and original values are available but collapsed",
        grepl("<details", more, fixed = TRUE) && !grepl("<details open", more, fixed = TRUE) && grepl("indicator-table", more))

  # ---- Exclusion: every output updates and agrees ----
  session$setInputs(exclude_low = TRUE)
  r <- rec()
  check("Switch on: 32 places ranked", nrow(r$X) == 32 && !any(LOW_CONF %in% rownames(r$X)))
  t3x <- text_of(output$top3)
  check("Top 3 updates and shows no left-out place",
        identical(top_k(r$topsis, 3)$State, c("Delhi", "Kerala", "Punjab")) && grepl("of 32 places ranked", t3x) &&
          !grepl("Chandigarh UT", t3x, fixed = TRUE))
  rk <- html_of(output$ranking)
  check("Full ranking has 32 rows", lengths(regmatches(rk, gregexpr("<tr>\\s*<td>", rk)))[1] == 32)
  check("Stability uses the 32 ranked places", !any(stab_sum()$places$State %in% LOW_CONF))
  session$setInputs(cmp_places = c("Lakshadweep", "Goa"))
  check("Compare uses the 32 ranked places; a left-out selection is explained",
        lengths(regmatches(html_of(output$tc_chart), gregexpr('class="ilm-state tc-pt', html_of(output$tc_chart), fixed = TRUE))) == 32 &&
          grepl("Lakshadweep left out of the current ranking", text_of(output$tc_cards)) && grepl("among the 32 places ranked", text_of(output$tc_cards)) &&
          identical(tc_data()$cards$Rank, r$topsis$results$Rank[r$topsis$results$State == "Goa"]))
  session$setInputs(cat_pick = "Lakshadweep")
  cd <- text_of(output$cat_detail)
  check("Details of a left-out place say so and mark its estimated score",
        grepl("Not in your current ranking", cd) && grepl("estimated: no indicator available", cd))
  session$setInputs(pref_Safety = "5", pref_apply = 4)
  check("A what-if with the switch on keeps the 32 places", nrow(rec()$X) == 32 && !is.null(whatif()) && !whatif()$unchanged)
  rep_ex <- paste(readLines(output$report, warn = FALSE), collapse = "\n")
  check("Report ranks the same 32 places", lengths(regmatches(rep_ex, gregexpr("<tr>\\s*<td>", rep_ex)))[1] == 32)
  mp <- html_of(output$map_ui)
  check("Map hover cards carry coverage notes (estimated Connectivity marked)", grepl("no measured data for Connectivity", mp))

  # ---- Secondary content ----
  check("Catalogue shows data coverage", grepl(">limited<", html_of(output$catalogue)))
  check("What matters to you uses plain words", grepl("What matters most to you", html_of(output$weights)) &&
          !grepl("eigenvector|Consistency ratio", html_of(output$weights)))
})
if (is.na(old_news)) Sys.unsetenv("LIVABILITY_NEWS") else Sys.setenv(LIVABILITY_NEWS = old_news)
cat(sprintf("\nChecks passed: %d   failed: %d\n", n_pass, n_fail))
cat("\n=== Server checks complete ===\n")
