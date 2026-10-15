test_that("ordinal numbers read correctly", {
  expect_equal(ordinal(c(1, 2, 3, 4, 11, 12, 13, 21, 22, 23, 101)),
               c("1st", "2nd", "3rd", "4th", "11th", "12th", "13th", "21st", "22nd", "23rd", "101st"))
})

test_that("rank change is positive when a State/UT moves up", {
  base <- topsis(XM, setNames(rep(1 / 7, 7), CRITERIA))
  fam  <- topsis(XM, run_ahp(PERSONAS[["Family"]])$weights)
  ch <- rank_change(base, fam)
  expect_equal(ch$Rank_Change, ch$Baseline_Rank - ch$Personal_Rank)
  expect_equal(sort(ch$Personal_Rank), sort(fam$results$Rank))
})

test_that("the three personas keep their documented top 3", {
  top3 <- function(a) {
    r <- topsis(XM, run_ahp(a)$weights)
    top_k(r, 3)$State
  }
  expect_equal(top3(PERSONAS[["Young Professional"]]), c("Delhi", "Chandigarh", "Goa"))
  expect_equal(top3(PERSONAS[["Family"]]), c("Himachal Pradesh", "Chandigarh", "Punjab"))
  expect_equal(top3(PERSONAS[["Retiree"]]), c("Lakshadweep", "Ladakh", "Karnataka"))
})

test_that("the dataset has the expected shape", {
  expect_equal(nrow(DATA), 36)
  expect_equal(dim(XM), c(36, 7))
  expect_false(anyNA(XM))
  expect_true(all(XM >= 0 & XM <= 1))
  expect_length(LOWC, 4)
})

test_that("the app's calculation reproduces the analysis and rejects incomplete answers", {
  rec <- compute_recommendation(setNames(as.integer(PERSONAS[["Family"]]), CRITERIA), XM, LOWC)
  expect_equal(top_k(rec$topsis, 3)$State, c("Himachal Pradesh", "Chandigarh", "Punjab"))
  expect_error(compute_recommendation(setNames(c(NA, rep(3L, 6)), CRITERIA), XM, LOWC), "seven answers")
})

test_that("leaving out low-confidence States/UTs ranks only the other 32", {
  rec <- compute_recommendation(EQUAL_ANSWERS, XM, LOWC, exclude_low = TRUE)
  expect_equal(nrow(rec$topsis$results), 32)
  expect_false(any(LOWC %in% rec$topsis$results$State))
})

test_that("explanations quote the real score and weight", {
  rec <- compute_recommendation(setNames(as.integer(PERSONAS[["Family"]]), CRITERIA), XM, LOWC)
  pb <- priority_breakdown(rec$topsis, rec$ahp$weights, "Himachal Pradesh")
  expect_equal(pb$Score, unname(XM["Himachal Pradesh", pb$Criterion]))
  expect_equal(pb$Weight, unname(rec$ahp$weights[pb$Criterion]))
  expect_error(priority_breakdown(rec$topsis, rec$ahp$weights, "Atlantis"), "Unknown State/UT")
})

test_that("random weight draws sum to 1 and are reproducible", {
  w <- run_ahp(PERSONAS[["Retiree"]])$weights
  W <- dirichlet_weights(w, 200, 50, seed = 3)
  expect_equal(unname(rowSums(W)), rep(1, 200), tolerance = 1e-12)
  expect_identical(W, dirichlet_weights(w, 200, 50, seed = 3))
})