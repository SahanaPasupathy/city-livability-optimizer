# ==============================================================================
# Sensitivity and robustness helpers
# ------------------------------------------------------------------------------
# Question: how much do the rankings change if the weights or the method change
# a little? A recommendation that flips with tiny changes cannot be trusted.
# ==============================================================================

#' Random weight vectors scattered around a base weight vector.
#'
#' Draws from a Dirichlet distribution with parameters concentration * w, so
#' every draw sums to 1 and the average draw equals w. A larger concentration
#' means smaller changes (the standard deviation of weight j is roughly
#' sqrt(w_j (1 - w_j) / (concentration + 1))).
#'
#' @param w named base weights (positive, any scale).
#' @param n number of random weight vectors.
#' @param concentration how tightly the draws stay around w (default 50).
#' @param seed random seed, so results can be reproduced.
#' @return n x length(w) matrix; each row sums to 1; columns named as w.
dirichlet_weights <- function(w, n = 1000, concentration = 50, seed = 1) {
  if (anyNA(w) || any(w <= 0)) stop("Base weights must be positive.", call. = FALSE)
  set.seed(seed)
  w <- w / sum(w)
  g <- vapply(w * concentration, function(a) stats::rgamma(n, shape = a),
              numeric(n))
  g <- matrix(g, nrow = n, dimnames = list(NULL, names(w)))
  g / rowSums(g)
}

#' How stable is each State/UT's rank when the weights are perturbed?
#'
#' @param X decision matrix (States/UTs x criteria, higher = better).
#' @param w base weights.
#' @param n number of random weight vectors; @param concentration see
#'   dirichlet_weights(); @param seed random seed.
#' @return list(table, top3_same, top1_same, spearman):
#'   table      data.frame per State/UT: Base_Rank, Mean_Rank, Rank_Low and
#'              Rank_High (5th and 95th percentile), Top1_Pct, Top3_Pct,
#'              Top10_Pct (percentage of draws in that group), ordered by
#'              Base_Rank;
#'   top3_same  % of draws whose top 3 is exactly the base top 3 (as a set);
#'   top1_same  % of draws with the same number 1 as the base;
#'   spearman   mean Spearman correlation between the base ranking and each draw.
rank_stability <- function(X, w, n = 1000, concentration = 50, seed = 1) {
  W <- dirichlet_weights(w, n, concentration, seed)
  base <- topsis(X, w)$results
  base_rank <- setNames(base$Rank, base$State)
  R <- vapply(seq_len(n), function(i) {
    r <- topsis(X, W[i, ])$results
    unname(r$Rank[match(rownames(X), r$State)])
  }, numeric(nrow(X)))
  rownames(R) <- rownames(X)
  base_top3 <- names(base_rank)[base_rank <= 3]
  pct <- function(f) 100 * rowMeans(f(R))
  tab <- data.frame(
    State     = rownames(X),
    Base_Rank = unname(base_rank[rownames(X)]),
    Mean_Rank = rowMeans(R),
    Rank_Low  = apply(R, 1, stats::quantile, probs = 0.05, names = FALSE),
    Rank_High = apply(R, 1, stats::quantile, probs = 0.95, names = FALSE),
    Top1_Pct  = pct(function(m) m == 1),
    Top3_Pct  = pct(function(m) m <= 3),
    Top10_Pct = pct(function(m) m <= 10),
    row.names = NULL, stringsAsFactors = FALSE)
  tab <- tab[order(tab$Base_Rank, tab$State), ]
  top3_same <- 100 * mean(apply(R, 2, function(r) setequal(rownames(X)[r <= 3], base_top3)))
  top1_same <- 100 * mean(R[names(base_rank)[base_rank == 1][1], ] == 1)
  rho <- mean(apply(R, 2, function(r) stats::cor(r, base_rank[rownames(X)], method = "spearman")))
  list(table = tab, top3_same = top3_same, top1_same = top1_same, spearman = rho)
}

#' Simple weighted-sum model (SAW): score = sum_j w_j * x_ij.
#' A deliberately simple alternative to TOPSIS, used as a cross-check.
#' @return data.frame(State, Score, Rank) with Rank 1 = best.
weighted_sum <- function(X, w) {
  w <- w[colnames(X)] / sum(w)
  s <- as.vector(X %*% w)
  data.frame(State = rownames(X), Score = s,
             Rank = rank(-s, ties.method = "min"), stringsAsFactors = FALSE)
}

#' Spearman rank correlation between two rankings of the same States/UTs.
#' @param a,b data.frames with State and Rank columns.
spearman_between <- function(a, b) {
  stats::cor(a$Rank, b$Rank[match(a$State, b$State)], method = "spearman")
}

#' Number of States/UTs shared by the top k of two rankings.
topk_shared <- function(a, b, k = 3) {
  length(intersect(a$State[a$Rank <= k], b$State[b$Rank <= k]))
}

#' Ranking when one criterion is ignored (its weight set to 0).
#' @return TOPSIS results data.frame.
rank_without <- function(X, w, criterion) {
  if (!criterion %in% names(w)) stop("Unknown criterion: ", criterion, call. = FALSE)
  w[criterion] <- 0
  topsis(X, w)$results
}