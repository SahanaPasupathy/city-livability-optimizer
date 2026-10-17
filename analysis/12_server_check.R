# ==============================================================================
# Server-level checks of the Shiny app (shiny::testServer, no browser needed)
# Run from the project root: source("analysis/12_server_check.R")
# Drives the CURRENT interface's inputs (input IDs of app/app.R) and checks that
# its outputs use the view models and agree with each other on scores and on
# the included/excluded places. When the redesigned interface replaces
# app/app.R, update the input/output names here (the expectations stay valid).
# Writes nothing.
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
old_news <- Sys.getenv("LIVABILITY_NEWS", NA)
Sys.unsetenv("LIVABILITY_NEWS")                       # check the default (off)

app <- shinyAppDir("app")       # also sources R/ into the global environment
d <- load_livability_data(); META <- load_indicator_metadata()
LOW_CONF <- limited_coverage_states(d, META)
testServer(app, {
  session$setInputs(start = 1)
  for (v in c(4, 3, 5, 2, 4, 1, 3)) session$setInputs(answer = list(step = state$step, value = v))
  check("Seven answers open the results", state$step == 8)
  top <- top_k(rec()$topsis, 3)$State
  check("Top 3 is Delhi, Kerala, Chandigarh", identical(top, c("Delhi", "Kerala", "Chandigarh")))
  h <- html_of(output$top3)
  check("Top 3 shows relative match, stability counts and coverage notes",
        grepl("Relative match: 0.79", h) && grepl("small changes to your answers", h) && grepl("Limited data coverage", h))
  check("Top 3 offers the top 3 without limited-coverage places",
        grepl("Without places with limited data coverage, your top 3 would be Delhi, Kerala, Punjab", h))
  check("No TOPSIS, D+/D- or Pareto wording on the Top 3 tab", !grepl("TOPSIS|D\\+|Pareto", h))

  # Trade-offs: plain summary from tradeoff_view(), on the user's top two criteria.
  check("Trade-offs start on the user's two most important criteria",
        identical(default_tradeoff_axes(rec()$ahp$weights), c("Health", "Education")))
  session$setInputs(atlas_x = "Health", atlas_y = "Education")
  a <- html_of(output$atlas)
  tv <- tradeoff_view(rec(), "Health", "Education")
  check("Trade-offs lead with plain sentences, then the chart",
        regexpr(tv$sentences[1], a, fixed = TRUE) > 0 && regexpr(tv$sentences[1], a, fixed = TRUE) < regexpr("<svg", a, fixed = TRUE))
  check("Trade-off text avoids 'Pareto'", !grepl("Pareto", paste(tv$sentences, collapse = " ")))

  # What-if through editing several answers, and going back.
  session$setInputs(edit_answers = 1)
  for (v in c(4, 5, 5, 2, 4, 5, 3)) session$setInputs(answer = list(step = state$step, value = v))
  m <- html_of(output$mode_note)
  check("Editing several answers explains what changed",
        grepl("What changed since your previous answers", m) && grepl("Safety from 3 to 5", m) && grepl("Environment from 1 to 5", m))
  f <- output$report
  rep_edit <- paste(readLines(f, warn = FALSE), collapse = "\n")
  check("The report includes what changed since the previous answers", grepl("What changed since my previous answers", rep_edit, fixed = TRUE))
  session$setInputs(restore_previous = 1)
  check("Go back restores the previous answers", identical(unname(state$answers), c(4L, 3L, 5L, 2L, 4L, 1L, 3L)))

  # Robustness wording comes from robustness_view().
  session$setInputs(rb_run = 1)
  rb_html <- html_of(output$robustness)
  check("Robustness headline and notes come from robustness_view()",
        grepl("stays first in", rb_html) && grepl("not probabilities", rb_html) && grepl("Limited data coverage", rb_html))

  # Exclusion: every output uses the same 32 places.
  session$setInputs(exclude_low = TRUE)
  r <- rec()
  ranked <- rownames(r$X)
  check("Switch on: 32 places ranked", length(ranked) == 32 && !any(LOW_CONF %in% ranked))
  check("Top 3 cards never show a left-out place", !any(vapply(LOW_CONF, function(s) grepl(sprintf(">%s<", s), html_of(output$top3), fixed = TRUE), TRUE)))
  rk <- html_of(output$ranking)
  check("Full ranking has 32 rows", lengths(regmatches(rk, gregexpr("<tr>\\s*<td>", rk)))[1] == 32)
  session$setInputs(rb_run = 2)
  check("Robustness uses the 32 ranked places", nrow(robust()$summary) == 32)
  session$setInputs(atlas_x = "Health", atlas_y = "Environment")
  check("Trade-offs use the 32 ranked places", !any(tradeoff_view(r, "Health", "Environment")$frontier$State %in% LOW_CONF))
  session$setInputs(cat_pick = "Lakshadweep")
  cd <- html_of(output$cat_detail)
  check("Details of a left-out place say so and mark its estimated score",
        grepl("Not in your current ranking", cd) && grepl("estimated: no indicator available", cd) && grepl("of 33)", cd, fixed = TRUE))
  session$setInputs(cat_pick = "Kerala")
  cd <- html_of(output$cat_detail)
  check("Details rank among the 32 and show original values", grepl("of 32)", cd, fixed = TRUE) && grepl("0.55 percent", cd))
  cat_html <- html_of(output$catalogue)
  check("Catalogue shows data coverage (limited / partial / complete)",
        grepl(">limited<", cat_html) && grepl(">partial<", cat_html) && grepl(">complete<", cat_html))
  session$setInputs(cmp_pick = c("Kerala", "Goa"))
  cmp <- html_of(output$compare)
  cv <- comparison_view(r, c("Kerala", "Goa"), d, META, LOW_CONF)
  check("Comparison shows plain trade-offs from comparison_view()", grepl(htmltools::htmlEscape(cv$tradeoffs[1]), cmp, fixed = TRUE))
  check("Comparison scores agree with the ranking",
        grepl(sprintf("%.3f (%d)", r$topsis$results$Closeness[r$topsis$results$State == "Kerala"],
                      r$topsis$results$Rank[r$topsis$results$State == "Kerala"]), cmp, fixed = TRUE))
  rep_ex <- paste(readLines(output$report, warn = FALSE), collapse = "\n")
  check("Report ranks the same 32 places and has no Pareto list",
        lengths(regmatches(rep_ex, gregexpr("<tr>\\s*<td>", rep_ex)))[1] == 32 && !grepl("Pareto", rep_ex))
  session$setInputs(wi_crit = "Safety", wi_value = "5")
  check("What-if tab uses the same switch", nrow(scenario()$scenario$X) == 32)

  # Content of the reference tabs and News default.
  check("News is off by default", grepl("off by default", html_of(output$news)))
})
if (is.na(old_news)) Sys.unsetenv("LIVABILITY_NEWS") else Sys.setenv(LIVABILITY_NEWS = old_news)
cat(sprintf("\nChecks passed: %d   failed: %d\n", n_pass, n_fail))
cat("\n=== Server checks complete ===\n")
