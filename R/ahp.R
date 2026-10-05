# ==============================================================================
# AHP (Analytic Hierarchy Process) functions
# ------------------------------------------------------------------------------
# Pipeline:  1-5 importance answers -> 7x7 pairwise matrix -> principal
#            eigenvector (weights) -> lambda_max -> CI -> CR.
# Used by the analysis scripts and, later, by the Shiny app.
# ==============================================================================

# Saaty's Random Index (RI) for matrices of size n = 1, 2, ..., 10.
SAATY_RI <- c(0, 0, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)

# Demonstration personas: importance (1 = not important ... 5 = extremely
# important) given to each criterion. These are ASSUMPTIONS that illustrate
# personalisation; the rankings they produce come only from the calculations.
PERSONAS <- list(
  "Young Professional" = c(Education = 2, Safety = 3, Health = 2, Housing = 4,
                           Economy = 5, Environment = 2, Connectivity = 5),
  "Family"             = c(Education = 5, Safety = 5, Health = 5, Housing = 3,
                           Economy = 3, Environment = 3, Connectivity = 2),
  "Retiree"            = c(Education = 1, Safety = 4, Health = 5, Housing = 3,
                           Economy = 2, Environment = 5, Connectivity = 2)
)

#' Turn seven 1-5 importance answers into a 7x7 AHP pairwise comparison matrix.
#'
#' Rule (the "difference rule"): let d = answer_i - answer_j.
#'   d = 0 -> 1 (equal), d = 1 -> 3 (moderately more important),
#'   d = 2 -> 5 (strongly), d = 3 -> 7 (very strongly), d = 4 -> 9 (extremely).
#' So a_ij = 1 + 2d when d >= 0, and the reciprocal 1 / (1 + 2|d|) when d < 0.
#' Every value therefore lies on Saaty's 1-9 scale and the matrix is reciprocal.
#'
#' @param answers numeric vector of seven integers 1-5; named by criterion
#'   (any order) or unnamed (then assumed to be in CRITERIA order).
#' @return 7x7 numeric matrix with CRITERIA as row and column names.
build_pairwise_matrix <- function(answers) {
  if (length(answers) != length(CRITERIA)) {
    stop("Expected ", length(CRITERIA), " answers, got ", length(answers), ".",
         call. = FALSE)
  }
  if (!is.null(names(answers))) {
    if (!all(CRITERIA %in% names(answers))) {
      stop("Answers must be named by all seven criteria.", call. = FALSE)
    }
    answers <- answers[CRITERIA]
  }
  if (anyNA(answers) || any(answers != round(answers)) ||
      any(answers < 1) || any(answers > 5)) {
    stop("Each answer must be a whole number from 1 to 5.", call. = FALSE)
  }
  diff_ij <- outer(unname(answers), unname(answers), "-")   # d = a_i - a_j
  M <- ifelse(diff_ij >= 0, 1 + 2 * diff_ij, 1 / (1 + 2 * abs(diff_ij)))
  dimnames(M) <- list(CRITERIA, CRITERIA)
  M
}

#' AHP priority weights and consistency measures for a pairwise matrix.
#'
#' Weights are the principal eigenvector (eigenvector of the largest
#' eigenvalue lambda_max), scaled to sum to 1.
#'   CI = (lambda_max - n) / (n - 1)      CR = CI / RI(n)
#' A matrix is acceptably consistent when CR <= 0.10.
#'
#' @param M square positive reciprocal matrix.
#' @return list(weights, lambda_max, CI, RI, CR, consistent).
ahp_weights <- function(M) {
  n <- nrow(M)
  if (n != ncol(M) || n < 2 || n > length(SAATY_RI)) {
    stop("M must be a square matrix with 2 to ", length(SAATY_RI), " rows.",
         call. = FALSE)
  }
  eig    <- eigen(M)
  k      <- which.max(Re(eig$values))
  lambda <- Re(eig$values[k])
  w      <- abs(Re(eig$vectors[, k]))
  w      <- w / sum(w)
  names(w) <- rownames(M)
  CI <- (lambda - n) / (n - 1)
  RI <- SAATY_RI[n]
  CR <- if (RI > 0) CI / RI else 0
  list(weights = w, lambda_max = lambda, CI = CI, RI = RI, CR = CR,
       consistent = CR <= 0.10)
}

#' Full AHP run: answers -> matrix -> weights and consistency.
#' @param answers see build_pairwise_matrix().
#' @return list(answers, matrix, weights, lambda_max, CI, RI, CR, consistent).
run_ahp <- function(answers) {
  M <- build_pairwise_matrix(answers)
  c(list(answers = answers, matrix = M), ahp_weights(M))
}

#' Independent check of the eigenvector using the power method.
#' Repeatedly multiplying by M converges to the principal eigenvector, so this
#' should agree with ahp_weights() to many decimals.
power_method_weights <- function(M, iterations = 1000) {
  w <- rep(1 / nrow(M), nrow(M))
  for (i in seq_len(iterations)) {
    w <- as.vector(M %*% w)
    w <- w / sum(w)
  }
  names(w) <- rownames(M)
  w
}

#' Validation checks for one AHP result; returns a data.frame(Check, Passed).
validate_ahp <- function(res) {
  M <- res$matrix
  n <- length(CRITERIA)
  saaty_values <- c(1:9, 1 / (2:9))
  data.frame(
    Check = c(
      "Matrix is 7 x 7",
      "Diagonal is all 1",
      "Reciprocal: a_ij x a_ji = 1",
      "All entries lie on Saaty's scale (1-9 and reciprocals)",
      "Weights are positive and sum to 1",
      "lambda_max >= n (as theory requires)",
      "Power-method weights agree with eigen() weights",
      "Consistency Ratio <= 0.10"
    ),
    Passed = c(
      all(dim(M) == n),
      all(diag(M) == 1),
      isTRUE(all.equal(unname(M * t(M)), matrix(1, n, n))),
      all(sapply(as.vector(M), function(v) any(abs(v - saaty_values) < 1e-9))),
      all(res$weights > 0) && abs(sum(res$weights) - 1) < 1e-9,
      res$lambda_max >= n - 1e-9,
      max(abs(power_method_weights(M) - res$weights)) < 1e-6,
      res$consistent
    ),
    stringsAsFactors = FALSE
  )
}