# ==============================================================================
# Livability Robustness Engine
# ------------------------------------------------------------------------------
# Question: if the user's preferences (or, where evidence allows, the input data)
# were a little different, how often would each State/UT still rank near the
# top? Every simulated ranking is computed with the project's own functions:
# run_ahp() turns ratings into weights and topsis() ranks (ties share the best
# rank, as in the baseline). Nothing here is a new scoring method.
#
# Preference uncertainty (choose one):
#  * "ratings"   Each of the seven 1-5 answers independently moves one point up
#                or down with probability p_change (half each way), clipped to
#                1-5; weights are recomputed by AHP. This models "my answer could
#                have been one point different". It can be simulated (Monte
#                Carlo, seeded) or enumerated exactly (all 3^7 = 2,187 shift
#                combinations weighted by their probability).
#  * "dirichlet" Weights drawn from Dirichlet(concentration x baseline weights)
#                (dirichlet_weights() in R/sensitivity.R): mean = baseline,
#                larger concentration = smaller changes. Criteria with weight 0
#                stay at 0. Same model as analysis/10_sensitivity.R.
#  * "none"      Baseline weights in every simulation.
# Data uncertainty (optional): only from explicit, documented bounds per score
# (data_uncertainty()); each bounded cell is drawn uniformly within its bounds.
# The repository has no evidence for such bounds, so the app does not use it.
# The results are conditional on these assumptions. They are not probabilities
# that a place is objectively the best place to live.
# ==============================================================================

#' Validate and normalise criterion weights.
#'
#' @param w numeric weights, one per criterion (named, or in column order).
#' @param criteria criterion names they must cover.
#' @return named weights summing to 1. Errors on NA, negative, non-finite,
#'   wrong length, unknown names or an all-zero vector.
validate_weights <- function(w, criteria) {
  if (!is.numeric(w) || length(w) != length(criteria)) {
    stop("Need one numeric weight per criterion (", length(criteria), ").", call. = FALSE)
  }
  if (!is.null(names(w))) {
    if (!setequal(names(w), criteria)) stop("Weights must be named by the criteria.", call. = FALSE)
    w <- w[criteria]
  } else {
    names(w) <- criteria
  }
  if (anyNA(w) || any(!is.finite(w)) || any(w < 0)) {
    stop("Weights must be finite and non-negative.", call. = FALSE)
  }
  if (sum(w) <= 0) stop("At least one weight must be positive.", call. = FALSE)
  w / sum(w)
}

#' Documented data-uncertainty bounds for criterion scores.
#'
#' Use only when the bounds come from evidence (for example published
#' confidence intervals or measurement error); `source` must say where. Cells
#' with NA bounds are treated as exact.
#'
#' @param lower,upper matrices with the same dimnames as the decision matrix.
#' @param source non-empty description of where the bounds come from.
#' @return an object of class "data_uncertainty".
data_uncertainty <- function(lower, upper, source) {
  if (!is.character(source) || length(source) != 1 || !nzchar(trimws(source))) {
    stop("Data uncertainty needs a documented source for its bounds.", call. = FALSE)
  }
  if (!is.matrix(lower) || !is.matrix(upper) || !identical(dim(lower), dim(upper)) ||
      !identical(dimnames(lower), dimnames(upper))) {
    stop("lower and upper must be matrices with the same dimensions and names.", call. = FALSE)
  }
  both <- !is.na(lower) & !is.na(upper)
  if (any(xor(is.na(lower), is.na(upper)))) stop("Give both bounds or neither for each cell.", call. = FALSE)
  if (any(lower[both] > upper[both])) stop("A lower bound is above its upper bound.", call. = FALSE)
  structure(list(lower = lower, upper = upper, source = source), class = "data_uncertainty")
}

# Draw one perturbed decision matrix (bounded cells uniform within bounds).
draw_data <- function(X, du) {
  lo <- du$lower[rownames(X), colnames(X), drop = FALSE]
  hi <- du$upper[rownames(X), colnames(X), drop = FALSE]
  cells <- which(!is.na(lo) & !is.na(hi))
  X[cells] <- stats::runif(length(cells), lo[cells], hi[cells])
  X
}

# Weighted quantile by the inverse empirical CDF (with equal weights this is
# R's quantile type 1). Used so Monte Carlo and exact results are summarised
# the same way.
weighted_quantile <- function(x, w, probs) {
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(probs, function(p) x[which(cw >= p - 1e-12)[1]], numeric(1))
}

#' All +-1 shifts of seven ratings and their probabilities (for exact results).
#' @param p_change probability that one rating moves (half up, half down).
#' @return list(shifts = matrix of -1/0/1, prob = probabilities summing to 1).
rating_shift_grid <- function(k, p_change) {
  g <- as.matrix(expand.grid(rep(list(-1:1), k)))
  pr <- apply(g, 1, function(s) prod(ifelse(s == 0, 1 - p_change, p_change / 2)))
  list(shifts = unname(g), prob = pr / sum(pr))
}

#' Run the robustness analysis.
#'
#' @param X decision matrix (States/UTs x criteria, 0-1, higher = better).
#' @param answers named 1-5 ratings (required for preference = "ratings").
#' @param weights named baseline weights; defaults to run_ahp(answers)$weights.
#' @param preference "ratings", "dirichlet" or "none".
#' @param n number of simulations (Monte Carlo).
#' @param seed random seed (results are reproducible for the same inputs).
#' @param p_change for "ratings": chance that each answer is one point off.
#' @param concentration for "dirichlet": larger = smaller weight changes.
#' @param exact for "ratings": enumerate all shift combinations instead of sampling.
#' @param data NULL or a data_uncertainty() object.
#' @param flags optional named character vector of data notes per State/UT.
#' @param competitive_threshold top-3 share above which a State/UT other than
#'   the baseline #1 is listed as a competitive alternative.
#' @return list(summary, baseline, competitive, influence, ranks, weights, prob,
#'   not_comparable, settings). `summary` has, per State/UT: baseline rank and
#'   closeness, share of simulations at rank 1 / in the top 3 / 5 / 10 (and
#'   counts for Monte Carlo), median, 5/25/75/95 % quantiles and mean of the
#'   simulated rank, and data notes.
robustness_analysis <- function(X, answers = NULL, weights = NULL,
                                preference = c("ratings", "dirichlet", "none"),
                                n = 1000, seed = 1, p_change = 0.25, concentration = 50,
                                exact = FALSE, data = NULL, flags = NULL,
                                competitive_threshold = 0.25) {
  preference <- match.arg(preference)
  if (!is.matrix(X) || !is.numeric(X) || is.null(rownames(X)) || is.null(colnames(X))) {
    stop("X must be a numeric matrix with State and criterion names.", call. = FALSE)
  }
  criteria <- colnames(X)
  if (!is.null(answers)) {
    if (length(answers) != length(criteria) || is.null(names(answers)) || !setequal(names(answers), criteria)) {
      stop("answers must be named by the criteria.", call. = FALSE)
    }
    answers <- answers[criteria]
  }
  if (preference == "ratings" && is.null(answers)) stop("The ratings method needs the answers.", call. = FALSE)
  if (is.null(weights)) {
    if (is.null(answers)) stop("Give answers or weights.", call. = FALSE)
    weights <- run_ahp(answers)$weights
  }
  w0 <- validate_weights(weights, criteria)
  if (!exact && (length(n) != 1 || !is.finite(n) || n < 1 || n != round(n) || n > 1e5)) {
    stop("n must be a whole number between 1 and 100000.", call. = FALSE)
  }
  if (length(seed) != 1 || !is.finite(seed)) stop("seed must be a single number.", call. = FALSE)
  if (preference == "ratings" && (!is.numeric(p_change) || p_change < 0 || p_change > 1)) {
    stop("p_change must be between 0 and 1.", call. = FALSE)
  }
  if (preference == "dirichlet" && (!is.numeric(concentration) || concentration <= 0)) {
    stop("concentration must be positive.", call. = FALSE)
  }
  if (exact && (preference != "ratings" || !is.null(data))) {
    stop("Exact enumeration is available for the ratings method without data uncertainty.", call. = FALSE)
  }
  if (!is.null(data) && !inherits(data, "data_uncertainty")) {
    stop("data must be created with data_uncertainty() (documented bounds).", call. = FALSE)
  }

  # States/UTs with a missing score cannot be ranked: they are reported, not imputed.
  incomplete <- rownames(X)[!stats::complete.cases(X)]
  Xc <- X[stats::complete.cases(X), , drop = FALSE]
  if (nrow(Xc) < 2) stop("At least two States/UTs need complete scores.", call. = FALSE)
  base <- topsis(Xc, w0)$results
  states <- base$State

  # ---- the weight vectors (and probabilities) to evaluate ---------------------
  rank_with <- function(w, Xs = Xc) {
    r <- topsis(Xs, w)$results
    r$Rank[match(states, r$State)]
  }
  if (preference == "ratings") {
    if (exact) {
      grid <- rating_shift_grid(length(criteria), p_change)
      shifts <- grid$shifts; prob <- grid$prob
    } else {
      set.seed(seed)
      u <- matrix(stats::runif(n * length(criteria)), n)
      shifts <- ifelse(u < p_change / 2, -1, ifelse(u < p_change, 1, 0))
      prob <- rep(1 / n, n)
    }
    A <- pmin(pmax(sweep(shifts, 2, unname(answers), "+"), 1), 5)
    colnames(A) <- criteria
    # Equal answer vectors give equal weights: compute each distinct one once.
    key <- apply(A, 1, paste, collapse = "")
    uk <- unique(key)
    Wu <- t(vapply(uk, function(k) {
      a <- setNames(as.integer(strsplit(k, "")[[1]]), criteria)
      run_ahp(a)$weights[criteria]
    }, numeric(length(criteria))))
    W <- Wu[match(key, uk), , drop = FALSE]
  } else if (preference == "dirichlet") {
    pos <- w0 > 0
    W <- matrix(0, n, length(criteria), dimnames = list(NULL, criteria))
    if (sum(pos) == 1) {
      W[, pos] <- 1
    } else {
      W[, pos] <- dirichlet_weights(w0[pos], n, concentration, seed)
    }
    prob <- rep(1 / n, n)
  } else {
    W <- matrix(w0, n, length(criteria), byrow = TRUE, dimnames = list(NULL, criteria))
    prob <- rep(1 / n, n)
  }
  rownames(W) <- NULL
  colnames(W) <- criteria

  # ---- rank every simulation with the existing TOPSIS -----------------------
  if (is.null(data)) {
    wkey <- apply(round(W, 12), 1, paste, collapse = ",")
    uw <- unique(wkey)
    Ru <- vapply(uw, function(k) rank_with(W[match(k, wkey), ]), numeric(length(states)))
    R <- t(Ru[, match(wkey, uw), drop = FALSE])
  } else {
    if (preference == "none") set.seed(seed)        # otherwise continue the same stream
    R <- t(vapply(seq_len(nrow(W)), function(i) rank_with(W[i, ], draw_data(Xc, data)),
                  numeric(length(states))))
  }
  R <- matrix(R, ncol = length(states), dimnames = list(NULL, states))

  # ---- summaries -----------------------------------------------------------
  share <- function(k) colSums((R <= k) * prob)
  counts <- !exact
  q <- t(vapply(states, function(s) weighted_quantile(R[, s], prob, c(0.05, 0.25, 0.5, 0.75, 0.95)), numeric(5)))
  summary <- data.frame(
    State = states, Base_Rank = base$Rank, Base_Closeness = base$Closeness,
    Top1_Prop = share(1), Top3_Prop = share(3), Top5_Prop = share(5), Top10_Prop = share(10),
    Top1_N = if (counts) colSums(R <= 1) else NA, Top3_N = if (counts) colSums(R <= 3) else NA,
    Top5_N = if (counts) colSums(R <= 5) else NA, Top10_N = if (counts) colSums(R <= 10) else NA,
    Rank_Q05 = q[, 1], Rank_Q25 = q[, 2], Rank_Median = q[, 3], Rank_Q75 = q[, 4], Rank_Q95 = q[, 5],
    Rank_Mean = colSums(R * prob),
    Data_Note = if (is.null(flags)) "" else ifelse(states %in% names(flags), unname(flags[states]), ""),
    row.names = NULL, stringsAsFactors = FALSE)
  summary$Data_Note[is.na(summary$Data_Note)] <- ""
  summary <- summary[order(-summary$Top3_Prop, summary$Base_Rank, summary$State), ]
  rownames(summary) <- NULL

  top_state <- base$State[order(base$Rank, base$State)][1]
  bt <- summary[summary$State == top_state, ]
  competitive <- summary[summary$State != top_state & summary$Top3_Prop >= competitive_threshold,
                         c("State", "Base_Rank", "Top3_Prop", "Top1_Prop", "Rank_Median")]

  # ---- which weights move the ranking (association in the simulations) ------
  varying <- apply(W, 2, function(x) stats::sd(x) > 1e-12)
  influence <- NULL
  if (preference != "none" && any(varying) && nrow(W) >= 10) {
    base_rank <- base$Rank
    disagreement <- apply(R, 1, function(r) 1 - suppressWarnings(stats::cor(r, base_rank, method = "spearman")))
    disagreement[is.na(disagreement)] <- 0
    wcor <- function(a, b) {
      if (stats::sd(a) < 1e-12 || stats::sd(b) < 1e-12) return(NA_real_)
      ra <- rank(a); rb <- rank(b)
      sw <- sum(prob); ma <- sum(prob * ra) / sw; mb <- sum(prob * rb) / sw
      sum(prob * (ra - ma) * (rb - mb)) / sqrt(sum(prob * (ra - ma)^2) * sum(prob * (rb - mb)^2))
    }
    influence <- data.frame(
      Criterion = criteria,
      Weight_SD = apply(W, 2, function(x) sqrt(sum(prob * (x - sum(prob * x))^2))),
      Rho_Top_Rank = vapply(criteria, function(cn) wcor(W[, cn], R[, top_state]), numeric(1)),
      Rho_Disagreement = vapply(criteria, function(cn) wcor(W[, cn], disagreement), numeric(1)),
      row.names = NULL, stringsAsFactors = FALSE)
    influence <- influence[order(-abs(influence$Rho_Top_Rank), influence$Criterion), ]
    rownames(influence) <- NULL
  }

  list(summary = summary,
       baseline = list(state = top_state, top1_prop = bt$Top1_Prop, top3_prop = bt$Top3_Prop,
                       rank_median = bt$Rank_Median, weights = w0, results = base),
       competitive = competitive, influence = influence,
       ranks = R, weights = W, prob = prob,
       not_comparable = incomplete,
       settings = list(preference = preference, n = nrow(W), exact = exact, seed = if (exact) NA else seed,
                       p_change = if (preference == "ratings") p_change else NA,
                       concentration = if (preference == "dirichlet") concentration else NA,
                       data = if (is.null(data)) "not modelled" else data$source,
                       competitive_threshold = competitive_threshold))
}

#' Typical size of the weight changes a Dirichlet concentration implies:
#' the standard deviation of each weight, sqrt(w (1 - w) / (concentration + 1)).
dirichlet_weight_sd <- function(w, concentration) sqrt(w * (1 - w) / (concentration + 1))
