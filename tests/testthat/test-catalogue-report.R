# Catalogue data, per-State data notes and the report's optional sections.
test_that("the catalogue shows the same ranks and scores as the ranking", {
  rec <- compute_recommendation(setNames(as.integer(PERSONAS[["Family"]]), CRITERIA), XM, LOWC)
  cat <- catalogue_data(rec, DATA)
  expect_equal(nrow(cat), 36)
  res <- rec$topsis$results
  expect_equal(cat$Rank[match(res$State, cat$State)], res$Rank)
  expect_equal(cat$Closeness[match(res$State, cat$State)], res$Closeness)
  expect_equal(unname(as.matrix(cat[match(rownames(XM), cat$State), CRITERIA])), unname(XM))
  expect_equal(head(cat$State, 3), c("Himachal Pradesh", "Chandigarh", "Punjab"))
})

test_that("States left out by the low-confidence switch stay in the catalogue without a rank", {
  rec <- compute_recommendation(EQUAL_ANSWERS, XM, LOWC, exclude_low = TRUE)
  cat <- catalogue_data(rec, DATA)
  expect_equal(nrow(cat), 36)
  expect_true(all(is.na(cat$Rank[cat$State %in% LOWC])))
  expect_equal(sort(cat$Rank[!is.na(cat$Rank)]), 1:32)
})

test_that("data notes come from the dataset", {
  n <- state_data_notes(DATA, META, "Lakshadweep")
  expect_setequal(n$missing$Indicator, c("Health_Worker_Density", "Kachha_Houses_Pct", "Villages_3G4G_Pct", "EVs_per_Lakh"))
  expect_equal(n$criteria_without_data, "Connectivity")
  expect_setequal(n$partial, c("Health", "Housing"))
  expect_true(n$low_confidence)
  k <- state_data_notes(DATA, META, "Kerala")
  expect_equal(nrow(k$missing), 0)
  expect_false(k$low_confidence)
  expect_error(state_data_notes(DATA, META, "Atlantis"), "Unknown")
})

test_that("the report's optional sections appear without disturbing the ranking table", {
  rec <- compute_recommendation(setNames(as.integer(PERSONAS[["Family"]]), CRITERIA), XM, LOWC)
  sc <- compute_recommendation(setNames(c(5L, 1L, 5L, 3L, 3L, 3L, 2L), CRITERIA), XM, LOWC)
  html <- build_report_html(rec, XM, LOWC, "preset: Family", setNames(DATA$Type, DATA$State),
                            shortlist = c("Kerala", "Goa"), scenario = sc, d = DATA, meta = META)
  expect_equal(lengths(regmatches(html, gregexpr("<tr>\\s*<td>", html)))[1], 36)
  expect_true(grepl("My comparison", html) && grepl("What-if scenario (hypothetical)", html, fixed = TRUE))
  # The 7-criteria Pareto list was replaced by plain trade-offs and per-place data notes.
  expect_false(grepl("Pareto", html))
  expect_true(grepl("Data notes for the places in this report", html, fixed = TRUE))
  expect_true(grepl("<strong>Chandigarh</strong>\\s*:\\s*Limited data coverage", html))
  expect_true(any(vapply(comparison_view(rec, c("Kerala", "Goa"), DATA, META)$tradeoffs,
                         function(t) grepl(htmltools::htmlEscape(t), html, fixed = TRUE), logical(1))))
  expect_true(grepl("Evidence: ", html) && grepl("Before you move, check", html) && grepl("Stability: stayed in your top 3", html))
  plain <- build_report_html(rec, XM, LOWC, "preset: Family", NULL)
  expect_false(grepl("My comparison", plain))
  expect_false(grepl("Data notes for the places", plain))
})

test_that("the report uses the same ranked places as the app (regression: similar places)", {
  rec <- compute_recommendation(setNames(as.integer(PERSONAS[["Retiree"]]), CRITERIA), XM, LOWC, exclude_low = TRUE)
  html <- build_report_html(rec, XM, LOWC, "questions", NULL, d = DATA, meta = META)
  sims <- regmatches(html, gregexpr("does not use your priorities\\): [^<]*", html))[[1]]
  expect_length(sims, 3)
  # No place left out of the ranking may be offered as a similar place.
  expect_false(any(vapply(LOWC, function(s) any(grepl(s, sims, fixed = TRUE)), logical(1))))
  top <- top_k(rec$topsis, 3)$State
  for (st in top) {
    expected <- similar_states(rec$X, st, 3)$State
    expect_true(any(vapply(sims, function(x) all(vapply(expected, grepl, logical(1), x = x, fixed = TRUE)), logical(1))))
  }
})
