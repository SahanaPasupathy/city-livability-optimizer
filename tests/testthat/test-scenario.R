# What-if scenarios (R/scenario.R) and their report section.
A0 <- setNames(c(4L, 3L, 5L, 2L, 4L, 1L, 3L), CRITERIA)

test_that("a scenario equals a fresh run of the recommendation engine", {
  sc <- run_scenario(A0, c(Safety = 5L), XM, LOWC)
  direct <- compute_recommendation(replace(A0, "Safety", 5L), XM, LOWC)
  expect_equal(sc$scenario$topsis$results, direct$topsis$results)
  expect_equal(sc$base$topsis$results, compute_recommendation(A0, XM, LOWC)$topsis$results)
  expect_equal(sc$changed, "Safety")
  expect_equal(unname(sc$scenario_answers["Safety"]), 5L)
})

test_that("rank changes are baseline rank minus scenario rank, and a new #1 is detected", {
  sc <- run_scenario(A0, c(Safety = 5L), XM, LOWC)
  expect_equal(sc$changes$Rank_Change, sc$changes$Baseline_Rank - sc$changes$Personal_Rank)
  ch <- sc$changes[sc$changes$State == "Lakshadweep", ]
  expect_equal(c(ch$Baseline_Rank, ch$Personal_Rank, ch$Rank_Change), c(5, 1, 4))
  expect_equal(sc$base_top, "Delhi"); expect_equal(sc$scenario_top, "Lakshadweep"); expect_true(sc$new_top)
  same <- run_scenario(A0, c(Safety = 3L), XM, LOWC)
  expect_length(same$changed, 0)
  expect_false(same$new_top)
  expect_true(all(same$changes$Rank_Change == 0))
})

test_that("scenarios are deterministic and use the same low-confidence setting as the baseline", {
  a <- run_scenario(A0, c(Health = 1L, Economy = 5L), XM, LOWC, exclude_low = TRUE)
  b <- run_scenario(A0, c(Health = 1L, Economy = 5L), XM, LOWC, exclude_low = TRUE)
  expect_identical(a, b)
  expect_equal(nrow(a$base$X), 32); expect_equal(nrow(a$scenario$X), 32)
  expect_false(any(LOWC %in% a$changes$State))
})

test_that("invalid scenarios are rejected", {
  expect_error(run_scenario(A0, c(Nonsense = 3L), XM, LOWC), "named by criterion")
  expect_error(run_scenario(A0, 3L, XM, LOWC), "named by criterion")
  expect_error(run_scenario(A0, c(Safety = 0L), XM, LOWC), "whole number from 1 to 5")
  expect_error(run_scenario(A0, c(Safety = 6L), XM, LOWC), "whole number from 1 to 5")
  expect_error(run_scenario(A0, c(Safety = 2.5), XM, LOWC), "whole number from 1 to 5")
  expect_error(run_scenario(A0, c(Safety = NA), XM, LOWC), "whole number from 1 to 5")
  expect_error(run_scenario(replace(A0, 1, NA), c(Safety = 5L), XM, LOWC), "All seven baseline answers")
  expect_error(run_scenario(A0[1:6], c(Safety = 5L), XM, LOWC), "All seven baseline answers")
})

test_that("the report includes a scenario, also when it ranks a different set of States/UTs", {
  base_ex <- compute_recommendation(A0, XM, LOWC, exclude_low = TRUE)
  sc_all <- compute_recommendation(replace(A0, "Safety", 5L), XM, LOWC, exclude_low = FALSE)
  html <- build_report_html(base_ex, XM, LOWC, scenario = sc_all)           # used to fail
  expect_true(grepl("What-if scenario (hypothetical)", html, fixed = TRUE))
  expect_true(grepl("<td>Lakshadweep</td>\\s*<td>not ranked</td>", html))   # excluded from the baseline
  expect_true(grepl("cover different places", html))
  expect_equal(lengths(regmatches(html, gregexpr("<tr>\\s*<td>", html)))[1], 32)  # ranking rows only
  sc <- run_scenario(A0, c(Safety = 5L), XM, LOWC)
  same <- build_report_html(sc$base, XM, LOWC, scenario = sc$scenario)
  expect_true(grepl("<td>Lakshadweep</td>\\s*<td>5</td>", same))
  expect_false(grepl("cover different places", same))
})
