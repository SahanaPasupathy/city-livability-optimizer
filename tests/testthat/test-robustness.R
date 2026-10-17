# Livability Robustness Engine (R/robustness.R).
A_CUSTOM <- setNames(c(4L, 3L, 5L, 2L, 4L, 1L, 3L), CRITERIA)

test_that("with no perturbation every simulation reproduces the baseline TOPSIS ranking", {
  r <- robustness_analysis(XM, A_CUSTOM, preference = "none", n = 25)
  base <- topsis(XM, run_ahp(A_CUSTOM)$weights)$results
  for (i in seq_len(nrow(r$ranks))) expect_equal(unname(r$ranks[i, base$State]), as.numeric(base$Rank))
  s <- r$summary[match(base$State, r$summary$State), ]
  expect_equal(s$Base_Rank, base$Rank)
  expect_equal(s$Base_Closeness, base$Closeness)
  expect_equal(s$Top3_Prop, as.numeric(base$Rank <= 3))
  expect_equal(s$Rank_Median, as.numeric(base$Rank))
  expect_equal(r$baseline$state, "Delhi")
  expect_equal(r$baseline$top1_prop, 1)
})

test_that("p_change = 0 (ratings) and weights given directly also reproduce the baseline", {
  base <- topsis(XM, run_ahp(PERSONAS[["Family"]])$weights)$results
  r <- robustness_analysis(XM, setNames(as.integer(PERSONAS[["Family"]]), CRITERIA), p_change = 0, n = 20)
  expect_true(all(abs(r$summary$Top1_Prop - round(r$summary$Top1_Prop)) < 1e-12))   # every share is 0 or 1
  expect_equal(r$baseline$state, base$State[base$Rank == 1])
  w <- robustness_analysis(XM, weights = run_ahp(PERSONAS[["Family"]])$weights, preference = "none", n = 5)
  expect_equal(w$summary$Base_Rank[match(base$State, w$summary$State)], base$Rank)
})

test_that("results are reproducible with a fixed seed and change with the seed", {
  a <- robustness_analysis(XM, A_CUSTOM, n = 300, seed = 11)
  b <- robustness_analysis(XM, A_CUSTOM, n = 300, seed = 11)
  c <- robustness_analysis(XM, A_CUSTOM, n = 300, seed = 12)
  expect_identical(a$summary, b$summary)
  expect_identical(a$ranks, b$ranks)
  expect_false(identical(a$ranks, c$ranks))
  d1 <- robustness_analysis(XM, A_CUSTOM, preference = "dirichlet", n = 200, seed = 5)
  d2 <- robustness_analysis(XM, A_CUSTOM, preference = "dirichlet", n = 200, seed = 5)
  expect_identical(d1$weights, d2$weights)
})

test_that("Dirichlet mode agrees with the existing rank_stability() analysis", {
  w <- run_ahp(PERSONAS[["Retiree"]])$weights
  r <- robustness_analysis(XM, weights = w, preference = "dirichlet", n = 300, seed = 2026, concentration = 50)
  s <- rank_stability(XM, w, n = 300, concentration = 50, seed = 2026)
  m <- merge(r$summary, s$table, by = "State")
  expect_equal(m$Top3_Prop * 100, m$Top3_Pct)
  expect_equal(m$Top1_Prop * 100, m$Top1_Pct)
  expect_equal(m$Top10_Prop * 100, m$Top10_Pct)
  expect_equal(r$baseline$top1_prop * 100, s$top1_same)
})

test_that("top-k counts, shares and rank summaries are computed from the simulated ranks", {
  r <- robustness_analysis(XM, A_CUSTOM, n = 400, seed = 3)
  R <- r$ranks
  for (st in c("Delhi", "Kerala", "Bihar")) {
    s <- r$summary[r$summary$State == st, ]
    expect_equal(s$Top1_N, sum(R[, st] <= 1)); expect_equal(s$Top3_N, sum(R[, st] <= 3))
    expect_equal(s$Top5_N, sum(R[, st] <= 5)); expect_equal(s$Top10_N, sum(R[, st] <= 10))
    expect_equal(s$Top3_Prop, mean(R[, st] <= 3))
    expect_equal(s$Rank_Mean, mean(R[, st]))
    expect_equal(s$Rank_Median, unname(quantile(R[, st], 0.5, type = 1)))
    expect_equal(s$Rank_Q95, unname(quantile(R[, st], 0.95, type = 1)))
    expect_true(s$Rank_Q05 <= s$Rank_Q25 && s$Rank_Q25 <= s$Rank_Median && s$Rank_Median <= s$Rank_Q75 && s$Rank_Q75 <= s$Rank_Q95)
  }
  expect_true(all(r$summary$Top3_Prop <= r$summary$Top5_Prop & r$summary$Top5_Prop <= r$summary$Top10_Prop))
  expect_equal(sum(r$summary$Top1_N), 400)          # no ties at rank 1 on this data
  expect_equal(r$baseline$top3_prop, mean(R[, "Delhi"] <= 3))
})

test_that("exact enumeration covers all 3^7 rating shifts and Monte Carlo converges to it", {
  ex <- robustness_analysis(XM, A_CUSTOM, exact = TRUE, p_change = 0.25)
  expect_equal(ex$settings$n, 3^7)
  expect_equal(sum(ex$prob), 1)
  expect_true(all(is.na(ex$summary$Top3_N)))         # shares only in exact mode
  mc <- robustness_analysis(XM, A_CUSTOM, n = 4000, seed = 9, p_change = 0.25)
  m <- merge(ex$summary, mc$summary, by = "State")
  se <- sqrt(m$Top3_Prop.x * (1 - m$Top3_Prop.x) / 4000)
  expect_true(all(abs(m$Top3_Prop.x - m$Top3_Prop.y) <= 4 * se + 1e-9))
  # A hand check: probability that no rating moves is (1 - p)^7.
  g <- rating_shift_grid(7, 0.25)
  expect_equal(g$prob[rowSums(abs(g$shifts)) == 0], 0.75^7)
})

test_that("ratings are clipped to 1-5 and run through the normal AHP validation", {
  top <- setNames(rep(5L, 7), CRITERIA)
  r <- robustness_analysis(XM, top, exact = TRUE, p_change = 0.5)
  expect_true(all(abs(rowSums(r$weights) - 1) < 1e-12))
  expect_true(all(r$weights > 0))
  # All-5 answers can only move down; all-1 only up: both reach equal weights again.
  expect_true(any(apply(r$weights, 1, function(w) all(abs(w - 1 / 7) < 1e-12))))
})

test_that("ties are handled exactly as in the baseline (shared best rank)", {
  X <- rbind(A = c(1, 1), B = c(1, 1), C = c(0, 0), D = c(0.5, 0.5))
  colnames(X) <- c("c1", "c2")
  r <- robustness_analysis(X, weights = c(c1 = 1, c2 = 1), preference = "dirichlet", n = 50, seed = 1)
  s <- r$summary
  expect_equal(s$Top1_Prop[s$State %in% c("A", "B")], c(1, 1))   # tied first in every draw
  expect_equal(s$Base_Rank[s$State == "B"], 1)
  expect_equal(r$baseline$state, "A")                            # ties broken by name for the label
  expect_equal(sort(unique(as.vector(r$ranks[, "D"]))), 3)
})

test_that("States with missing scores are reported as not comparable, never imputed", {
  X <- XM; X["Goa", "Health"] <- NA
  r <- robustness_analysis(X, A_CUSTOM, n = 50, seed = 1)
  expect_equal(r$not_comparable, "Goa")
  expect_false("Goa" %in% r$summary$State)
  expect_equal(nrow(r$summary), 35)
  expect_equal(max(r$ranks), 35)
})

test_that("data notes are carried through without changing any number", {
  flags <- c(Lakshadweep = "no Connectivity data", Chandigarh = "low confidence")
  a <- robustness_analysis(XM, A_CUSTOM, n = 100, seed = 4)
  b <- robustness_analysis(XM, A_CUSTOM, n = 100, seed = 4, flags = flags)
  expect_equal(b$summary$Data_Note[b$summary$State == "Lakshadweep"], "no Connectivity data")
  expect_equal(b$summary$Data_Note[b$summary$State == "Kerala"], "")
  expect_equal(a$summary$Top3_Prop, b$summary$Top3_Prop)
})

test_that("data uncertainty needs documented bounds and is separate from preference uncertainty", {
  expect_error(data_uncertainty(XM, XM, ""), "documented source")
  lo <- XM; hi <- XM
  expect_error(data_uncertainty(hi + 0.1, lo, "test"), "lower bound is above")
  lo[] <- NA; hi[] <- NA
  lo["Delhi", "Safety"] <- 0; hi["Delhi", "Safety"] <- 0.6         # one wide, documented interval
  du <- data_uncertainty(lo, hi, "synthetic bounds for testing")
  expect_error(robustness_analysis(XM, A_CUSTOM, data = list(lower = lo)), "data_uncertainty")
  only_data <- robustness_analysis(XM, A_CUSTOM, preference = "none", data = du, n = 300, seed = 2)
  expect_true(all(abs(sweep(only_data$weights, 2, run_ahp(A_CUSTOM)$weights, "-")) < 1e-12))  # weights fixed
  expect_true(only_data$summary$Top1_Prop[only_data$summary$State == "Delhi"] < 1)           # data moved Delhi
  expect_null(only_data$influence)                                                           # no weight variation
  expect_equal(only_data$settings$data, "synthetic bounds for testing")
  unbounded <- robustness_analysis(XM, A_CUSTOM, preference = "none", n = 50)
  expect_equal(unbounded$settings$data, "not modelled")
  expect_error(robustness_analysis(XM, A_CUSTOM, exact = TRUE, data = du), "Exact enumeration")
})

test_that("invalid inputs and zero weights are handled", {
  w <- run_ahp(A_CUSTOM)$weights
  expect_error(validate_weights(c(1, 2), CRITERIA), "one numeric weight")
  expect_error(validate_weights(setNames(rep(0, 7), CRITERIA), CRITERIA), "positive")
  expect_error(validate_weights(replace(w, 1, -0.1), CRITERIA), "non-negative")
  expect_error(validate_weights(replace(w, 1, NA), CRITERIA), "non-negative")
  expect_equal(sum(validate_weights(w * 7, CRITERIA)), 1)
  expect_error(robustness_analysis(XM, preference = "ratings", weights = w), "needs the answers")
  expect_error(robustness_analysis(XM, A_CUSTOM, n = 0), "whole number")
  expect_error(robustness_analysis(XM, A_CUSTOM, n = 2.5), "whole number")
  expect_error(robustness_analysis(XM, A_CUSTOM, p_change = 1.5), "between 0 and 1")
  expect_error(robustness_analysis(XM, A_CUSTOM, preference = "dirichlet", concentration = 0), "positive")
  expect_error(robustness_analysis(XM, A_CUSTOM[1:6]), "named by the criteria")
  # A zero weight stays zero under Dirichlet perturbation.
  wz <- replace(w, "Environment", 0)
  z <- robustness_analysis(XM, weights = wz, preference = "dirichlet", n = 100, seed = 1)
  expect_true(all(z$weights[, "Environment"] == 0))
  expect_true(all(abs(rowSums(z$weights) - 1) < 1e-12))
})

test_that("competitive alternatives and weight influence are derived from the simulations", {
  r <- robustness_analysis(XM, A_CUSTOM, n = 1000, seed = 7)
  expect_false(r$baseline$state %in% r$competitive$State)
  expect_true(all(r$competitive$Top3_Prop >= 0.25))
  expect_setequal(r$competitive$State, r$summary$State[r$summary$State != "Delhi" & r$summary$Top3_Prop >= 0.25])
  expect_equal(nrow(r$influence), 7)
  expect_true(all(abs(r$influence$Rho_Top_Rank) <= 1, na.rm = TRUE))
  # Spearman between each simulated weight and Delhi's simulated rank, recomputed directly.
  for (cn in r$influence$Criterion[1:2]) {
    expect_equal(r$influence$Rho_Top_Rank[r$influence$Criterion == cn],
                 cor(r$weights[, cn], r$ranks[, "Delhi"], method = "spearman"), tolerance = 1e-10)
  }
})

test_that("simulation is fast enough for interactive use", {
  t <- system.time(robustness_analysis(XM, A_CUSTOM, n = 2000, seed = 1))[["elapsed"]]
  expect_lt(t, 10)
  t2 <- system.time(robustness_analysis(XM, A_CUSTOM, preference = "dirichlet", n = 2000, seed = 1))[["elapsed"]]
  expect_lt(t2, 10)
})
