# ==============================================================================
# TOPSIS (Technique for Order Preference by Similarity to Ideal Solution)
# ------------------------------------------------------------------------------
# Input : decision matrix X (alternatives x criteria, all "higher = better") and
#         criterion weights w (from AHP).
# Steps : (1) normalise  (2) weight  (3) positive & negative ideal
#         (4) Euclidean distances D+ and D-  (5) closeness  (6) rank.
# ==============================================================================

#' Run TOPSIS.
#'
#' Normalisation: our criterion scores are already min-max scaled to 0-1 and are
#' directly comparable, so the default is `normalise = "none"` (r_ij = x_ij);
#' applying vector normalisation on top would scale the data a second time.
#' `normalise = "vector"` (r_ij = x_ij / sqrt(sum_i x_ij^2)) is the textbook
#' alternative and is used later as a robustness check.
#'
#' With every criterion "higher = better":
#'   weighted matrix   v_ij = w_j * r_ij
#'   positive ideal    A+_j = max_i v_ij      negative ideal  A-_j = min_i v_ij
#'   D+_i = sqrt(sum_j (v_ij - A+_j)^2)       D-_i = sqrt(sum_j (v_ij - A-_j)^2)
#'   closeness         C_i  = D-_i / (D+_i + D-_i)    (higher = better)
#'
#' @param X numeric matrix, alternatives (rows, named) x criteria (columns).
#' @param w non-negative criterion weights; named (any order) or in column
#'   order. They are re-scaled to sum to 1.
#' @param normalise "none" (default) or "vector".
#' @return list(decision, normalised, weighted, ideal, anti_ideal, results)
#'   where `results` has State, D_plus, D_minus, Closeness, Rank (1 = best).
topsis <- function(X, w, normalise = c("none", "vector")) {
  normalise <- match.arg(normalise)
  if (!is.matrix(X) || !is.numeric(X)) stop("X must be a numeric matrix.", call. = FALSE)
  if (anyNA(X)) stop("X must not contain missing values.", call. = FALSE)
  if (length(w) != ncol(X)) stop("Need one weight per criterion.", call. = FALSE)
  if (!is.null(names(w))) w <- w[colnames(X)]
  if (anyNA(w) || any(w < 0) || sum(w) <= 0) {
    stop("Weights must be non-negative and not all zero.", call. = FALSE)
  }
  w <- unname(w) / sum(w)
  
  if (normalise == "vector") {
    denom <- sqrt(colSums(X^2))
    denom[denom == 0] <- 1
    R <- sweep(X, 2, denom, "/")
  } else {
    R <- X
  }
  V <- sweep(R, 2, w, "*")
  
  ideal      <- apply(V, 2, max)
  anti_ideal <- apply(V, 2, min)
  d_plus     <- sqrt(rowSums(sweep(V, 2, ideal,      "-")^2))
  d_minus    <- sqrt(rowSums(sweep(V, 2, anti_ideal, "-")^2))
  closeness  <- d_minus / (d_plus + d_minus)
  
  results <- data.frame(
    State     = rownames(X),
    D_plus    = d_plus,
    D_minus   = d_minus,
    Closeness = closeness,
    Rank      = rank(-closeness, ties.method = "min"),
    row.names = NULL, stringsAsFactors = FALSE
  )
  list(decision = X, normalised = R, weighted = V,
       ideal = ideal, anti_ideal = anti_ideal, results = results)
}