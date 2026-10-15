toy <- rbind(A = c(1, 1), B = c(0, 0), C = c(1, 0))
colnames(toy) <- c("c1", "c2")

test_that("TOPSIS matches a hand calculation", {
  res <- topsis(toy, c(0.5, 0.5))$results
  r <- function(s) res[res$State == s, ]
  expect_equal(r("A")$Closeness, 1)               # sits on the ideal
  expect_equal(r("B")$Closeness, 0)               # sits on the worst case
  expect_equal(r("C")$Closeness, 0.5)             # exactly in between
  expect_equal(r("C")$D_plus, 0.5)
  expect_equal(r("C")$D_minus, 0.5)
  expect_equal(r("A")$Rank, 1); expect_equal(r("C")$Rank, 2); expect_equal(r("B")$Rank, 3)
})

test_that("weights are re-scaled and may be given in any order", {
  a <- topsis(toy, c(c1 = 2, c2 = 2))$results
  b <- topsis(toy, c(c2 = 1, c1 = 1))$results
  expect_equal(a$Closeness, b$Closeness)
})

test_that("a heavier weight on a criterion helps States/UTs strong on it", {
  X2 <- rbind(P = c(1, 0), Q = c(0, 1))
  colnames(X2) <- c("c1", "c2")
  expect_equal(topsis(X2, c(0.9, 0.1))$results$Rank, c(1, 2))
  expect_equal(topsis(X2, c(0.1, 0.9))$results$Rank, c(2, 1))
})

test_that("closeness always lies between 0 and 1 on the real data", {
  cl <- topsis(XM, setNames(rep(1 / 7, 7), CRITERIA))$results$Closeness
  expect_true(all(cl >= 0 & cl <= 1))
  expect_length(cl, 36)
})

test_that("bad inputs are rejected", {
  expect_error(topsis(toy, c(1, 1, 1)), "one weight per criterion")
  expect_error(topsis(toy, c(0, 0)), "not all zero")
  expect_error(topsis(toy, c(-1, 2)), "non-negative")
  bad <- toy; bad[1, 1] <- NA
  expect_error(topsis(bad, c(1, 1)), "missing values")
})