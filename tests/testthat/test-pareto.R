# Pareto efficiency (R/pareto.R).
pm <- function(...) {
  m <- rbind(...)
  colnames(m) <- c("a", "b")[seq_len(ncol(m))]
  m
}

test_that("a hand-checked two-criterion case is classified correctly", {
  X <- pm(P = c(0.9, 0.1), Q = c(0.5, 0.5), R = c(0.1, 0.9), S = c(0.4, 0.4), T = c(0.5, 0.2))
  f <- pareto_front(X, c("a", "b"))
  expect_equal(f$State[f$Efficient], c("P", "Q", "R"))
  expect_equal(attr(f, "dominated_by")[["S"]], "Q")        # Q >= S on both, better on both
  expect_setequal(attr(f, "dominated_by")[["T"]], c("Q"))  # P is better on a but worse on b
  expect_equal(f$Status[f$State == "S"], "dominated")
})

test_that("ties: identical points do not dominate each other", {
  X <- pm(A = c(0.7, 0.7), B = c(0.7, 0.7), C = c(0.2, 0.2))
  f <- pareto_front(X, c("a", "b"))
  expect_equal(f$Efficient, c(TRUE, TRUE, FALSE))
  expect_setequal(attr(f, "dominated_by")[["C"]], c("A", "B"))
})

test_that("equal on one criterion and better on the other is enough to dominate", {
  X <- pm(A = c(0.5, 0.8), B = c(0.5, 0.6))
  f <- pareto_front(X, c("a", "b"))
  expect_equal(f$Efficient, c(TRUE, FALSE))
})

test_that("direction of preference is respected", {
  X <- pm(A = c(0.9, 0.9), B = c(0.1, 0.1))
  expect_equal(pareto_front(X, c("a", "b"))$Efficient, c(TRUE, FALSE))
  expect_equal(pareto_front(X, c("a", "b"), higher_better = FALSE)$Efficient, c(FALSE, TRUE))
  # Mixed: a higher is better, b lower is better.
  Y <- pm(A = c(0.9, 0.1), B = c(0.9, 0.5))
  expect_equal(pareto_front(Y, c("a", "b"), higher_better = c(TRUE, FALSE))$Efficient, c(TRUE, FALSE))
})

test_that("missing values are not classified and never dominate", {
  X <- pm(A = c(NA, 1), B = c(0.5, 0.5), C = c(0.2, 0.2))
  f <- pareto_front(X, c("a", "b"))
  expect_true(is.na(f$Efficient[f$State == "A"]))
  expect_equal(f$Status[f$State == "A"], "insufficient data")
  expect_true(f$Efficient[f$State == "B"])                  # A cannot dominate B
  expect_match(explain_pareto(f, "A"), "cannot be assessed")
})

test_that("a single criterion leaves only the maximum (or tied maxima) efficient", {
  X <- pm(A = c(0.3, 0), B = c(0.9, 0), C = c(0.9, 0))
  f <- pareto_front(X, "a")
  expect_equal(f$State[f$Efficient], c("B", "C"))
})

test_that("results agree with a brute-force definition on random data", {
  set.seed(42)
  for (k in 1:20) {
    X <- matrix(round(runif(30 * 3), 1), 30, 3, dimnames = list(paste0("s", 1:30), c("a", "b", "c")))
    f <- pareto_front(X, c("a", "b", "c"))
    brute <- vapply(seq_len(30), function(i) {
      !any(vapply(seq_len(30)[-i], function(j) all(X[j, ] >= X[i, ]) && any(X[j, ] > X[i, ]), logical(1)))
    }, logical(1))
    expect_equal(f$Efficient, brute)
  }
})

test_that("on the real data, every efficient place is undominated and every dominated place has a dominator", {
  for (pair in list(c("Safety", "Economy"), c("Health", "Environment"), CRITERIA)) {
    f <- pareto_front(XM, pair)
    dom <- attr(f, "dominated_by")
    expect_true(all(lengths(dom[f$State[f$Efficient]]) == 0))
    expect_true(all(lengths(dom[f$State[!f$Efficient]]) > 0))
    for (st in f$State[!f$Efficient]) {
      j <- dom[[st]][1]
      expect_true(all(XM[j, pair] >= XM[st, pair]) && any(XM[j, pair] > XM[st, pair]))
    }
  }
})

test_that("rescaling a criterion does not change who is efficient", {
  f1 <- pareto_front(XM, c("Safety", "Economy"))
  Y <- XM; Y[, "Safety"] <- 10 * Y[, "Safety"] + 3
  expect_equal(pareto_front(Y, c("Safety", "Economy"))$Efficient, f1$Efficient)
})

test_that("the frontier is ordered left to right and bad inputs are rejected", {
  f <- pareto_front(XM, c("Safety", "Economy"))
  ord <- pareto_frontier_order(XM, f)
  expect_false(is.unsorted(XM[ord, "Safety"]))
  expect_true(all(diff(XM[ord, "Economy"]) <= 0))           # moving right costs on the other axis
  expect_error(pareto_front(XM, "Nonsense"), "Unknown criteria")
  expect_error(pareto_front(unname(XM)), "row names")
})
