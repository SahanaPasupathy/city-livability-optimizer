test_that("pairwise matrix follows the difference rule", {
  M <- build_pairwise_matrix(setNames(c(5, 1, 3, 3, 3, 3, 4), CRITERIA))
  expect_equal(M["Education", "Safety"], 9)       # 5 vs 1: four apart -> 9
  expect_equal(M["Safety", "Education"], 1 / 9)   # reciprocal
  expect_equal(M["Education", "Connectivity"], 3) # 5 vs 4: one apart -> 3
  expect_equal(M["Health", "Housing"], 1)         # equal answers -> 1
  expect_equal(unname(diag(M)), rep(1, 7))
})

test_that("a perfectly consistent matrix returns its own weights and CR = 0", {
  w <- c(a = 0.5, b = 0.3, c = 0.2)
  M <- outer(w, 1 / w)                            # a_ij = w_i / w_j
  res <- ahp_weights(M)
  expect_equal(unname(res$weights), unname(w), tolerance = 1e-9)
  expect_equal(res$lambda_max, 3, tolerance = 1e-9)
  expect_equal(res$CR, 0, tolerance = 1e-9)
})

test_that("Saaty's textbook 3x3 example gives the published weights", {
  M <- matrix(c(1, 3, 5,  1 / 3, 1, 3,  1 / 5, 1 / 3, 1), 3, byrow = TRUE,
              dimnames = list(c("x", "y", "z"), c("x", "y", "z")))
  res <- ahp_weights(M)
  expect_equal(unname(round(res$weights, 3)), c(0.637, 0.258, 0.105))
  expect_equal(round(res$lambda_max, 3), 3.039)
  expect_true(res$consistent)
})

test_that("a clearly contradictory matrix is flagged inconsistent", {
  # A beats B, B beats C, C beats A, all strongly.
  M <- matrix(c(1, 9, 1 / 9,  1 / 9, 1, 9,  9, 1 / 9, 1), 3, byrow = TRUE,
              dimnames = list(letters[1:3], letters[1:3]))
  expect_false(ahp_weights(M)$consistent)
})

test_that("equal answers give equal weights; higher answers give higher weights", {
  eq <- run_ahp(setNames(rep(3, 7), CRITERIA))
  expect_equal(unname(eq$weights), rep(1 / 7, 7), tolerance = 1e-12)
  expect_equal(eq$CR, 0, tolerance = 1e-12)
  a <- run_ahp(setNames(c(5, 4, 3, 3, 2, 2, 1), CRITERIA))
  expect_false(is.unsorted(rev(unname(a$weights))))
  expect_equal(sum(a$weights), 1, tolerance = 1e-12)
})

test_that("wrong inputs are rejected", {
  expect_error(build_pairwise_matrix(c(1, 2, 3)), "Expected 7 answers")
})

test_that("every possible answer set is consistent (CR stays below 0.10)", {
  set.seed(1)
  crs <- replicate(300, run_ahp(sample(1:5, 7, replace = TRUE))$CR)
  expect_true(all(crs <= 0.10))
})