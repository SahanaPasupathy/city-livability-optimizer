# ==============================================================================
# Top-k recommendation and a dynamically generated explanation.
# Every sentence is built from the numbers of THIS user's run (their weights,
# the State/UT's scores, its TOPSIS distances), so the text changes whenever
# the answers change. Nothing is hard-coded per State/UT or per persona.
# ==============================================================================

#' Criterion names ordered by weight, largest first.
#' Weights are rounded to 12 decimals before ordering, so criteria the user
#' rated equally (whose weights differ only by floating-point noise, which can
#' vary between machines) keep the fixed criterion order instead of a random one.
#' @param w named weights.
#' @return character vector of names.
by_weight <- function(w) {
  names(w)[order(-round(w, 12), seq_along(w))]
}

#' 1 -> "1st", 2 -> "2nd", 11 -> "11th" ...
ordinal <- function(n) {
  suffix <- ifelse(n %% 100 %in% 11:13, "th",
                   ifelse(n %% 10 == 1, "st",
                          ifelse(n %% 10 == 2, "nd",
                                 ifelse(n %% 10 == 3, "rd", "th"))))
  paste0(n, suffix)
}

#' The k best-ranked States/UTs from a topsis() result.
#' @param res list returned by topsis().
#' @param k number of States/UTs to return.
#' @return the rows of res$results with Rank <= k, best first.
top_k <- function(res, k = 3) {
  r <- res$results[order(res$results$Rank), ]
  r[seq_len(min(k, nrow(r))), ]
}

#' Explain why a State/UT received its rank.
#'
#' Definitions used (all visible in the text):
#'  * "matters to you"  = criterion whose weight is above an equal share (1/7).
#'  * "strong"          = the State/UT is in the top third of the places ranked
#'                        on that criterion; "weak" = bottom third.
#'  * "shortfall"       = w_j * (1 - score_j): how many weighted points the
#'                        State/UT loses against a perfect score on criterion j.
#'                        It is also the criterion that adds most to D+.
#'
#' @param res list returned by topsis().
#' @param w named AHP weights (any order; re-scaled to sum to 1).
#' @param state State/UT to explain.
#' @param low_confidence character vector of States/UTs with thin data.
#' @return character vector, one sentence per element.
explain_state <- function(res, w, state, low_confidence = character(0)) {
  X <- res$decision
  if (!state %in% rownames(X)) stop("Unknown State/UT: ", state, call. = FALSE)
  w <- w[colnames(X)] / sum(w)
  n <- nrow(X); p <- ncol(X)
  row <- res$results[res$results$State == state, ]
  
  score <- X[state, ]
  # Position of this State/UT among all States/UTs on each criterion (1 = best).
  pos <- sapply(colnames(X), function(cn) {
    unname(rank(-X[, cn], ties.method = "min")[state])
  })
  fmt_item <- function(cn) sprintf("%s (relative score %.2f, %s of %d)",
                                   cn, score[cn], ordinal(pos[cn]), n)
  
  important <- colnames(X)[w > 1 / p]
  important <- intersect(by_weight(w), important)
  strong    <- important[pos[important] <= ceiling(n / 3)]
  weak      <- important[pos[important] > n - ceiling(n / 3)]
  others    <- setdiff(colnames(X), important)
  bonus     <- others[pos[others] <= 5]
  
  top_w <- by_weight(w)[1:3]
  shortfall <- w * (1 - score)
  worst <- names(which.max(shortfall))
  
  out <- character(0)
  out <- c(out, sprintf(
    "%s ranks %s of %d for your priorities (relative match %.2f, where 1 means closest to the best of the places ranked).",
    state, ordinal(row$Rank), n, row$Closeness))
  out <- c(out, sprintf("What matters most to you: %s.",
                        paste(sprintf("%s (%.0f%%)", top_w, 100 * w[top_w]), collapse = ", ")))
  out <- c(out, if (length(strong) > 0)
    sprintf("In the top third where it matters to you: %s.", paste(sapply(strong, fmt_item), collapse = "; "))
    else "It is not in the top third of the places ranked on any criterion that matters most to you.")
  if (length(weak) > 0) {
    out <- c(out, sprintf("In the bottom third on your priorities: %s.",
                          paste(sapply(weak, fmt_item), collapse = "; ")))
  }
  if (length(bonus) > 0) {
    out <- c(out, sprintf("Also near the top on criteria you weighted less: %s.",
                          paste(sapply(bonus, fmt_item), collapse = "; ")))
  }
  out <- c(out, sprintf(
    "Its biggest gap for you is %s: relative score %.2f, and %s carries %.0f%% of your weight.",
    worst, score[worst], worst, 100 * w[worst]))
  if (state %in% low_confidence) {
    out <- c(out, "Caution: this State/UT has limited data coverage (several indicators are missing), so treat its position as indicative.")
  }
  out
}

#' "Why this fits you": how a State/UT performs on the user's top priorities.
#'
#' Takes the user's `k` highest-weighted criteria and, for each, reports the
#' weight, the State/UT's score and its position among the places ranked, a
#' verdict and one sentence built from those real numbers.
#' Verdicts are POSITIONS, not quality judgements (scores are relative 0-1
#' values, so a fixed score threshold would imply more than the data can say):
#'   "Top third"    position <= ceiling(n / 3)
#'   "Bottom third" position >  n - ceiling(n / 3)
#'   "Middle third" otherwise.
#'
#' @param res list returned by topsis().
#' @param w named AHP weights (any order; re-scaled to sum to 1).
#' @param state State/UT to explain.
#' @param k number of top priorities to report.
#' @return data.frame(Criterion, Weight, Score, Position, Verdict, Sentence).
priority_breakdown <- function(res, w, state, k = 3) {
  X <- res$decision
  if (!state %in% rownames(X)) stop("Unknown State/UT: ", state, call. = FALSE)
  w <- w[colnames(X)] / sum(w)
  n <- nrow(X)
  top <- by_weight(w)[seq_len(min(k, length(w)))]
  score <- X[state, top]
  pos <- sapply(top, function(cn) unname(rank(-X[, cn], ties.method = "min")[state]))
  verdict <- position_group(pos, n)
  phrase <- c("Top third"    = "in the top third of the places ranked",
              "Middle third" = "in the middle third of the places ranked",
              "Bottom third" = "in the bottom third of the places ranked")
  data.frame(
    Criterion = top, Weight = unname(w[top]), Score = unname(score),
    Position = unname(pos), Verdict = unname(verdict),
    Sentence = sprintf("%s carries %.0f%% of your preference weight. %s is %s of %d on %s (relative score %.2f/1.00), %s.",
                       top, 100 * w[top], state, ordinal(pos), n, top, score,
                       phrase[verdict]),
    row.names = NULL, stringsAsFactors = FALSE)
}

#' Position group of a rank among n places: "Top third", "Middle third" or "Bottom third".
position_group <- function(pos, n) {
  third <- ceiling(n / 3)
  unname(ifelse(pos <= third, "Top third", ifelse(pos > n - third, "Bottom third", "Middle third")))
}

#' Where a State/UT falls furthest short of a perfect score, weighted by what the
#' user cares about: the criterion with the largest w_j * (1 - score_j).
#'
#' @param res list returned by topsis().
#' @param w named AHP weights (any order; re-scaled to sum to 1).
#' @param state State/UT to explain.
#' @return one sentence.
shortfall_sentence <- function(res, w, state) {
  X <- res$decision
  if (!state %in% rownames(X)) stop("Unknown State/UT: ", state, call. = FALSE)
  w <- w[colnames(X)] / sum(w)
  loss <- w * (1 - X[state, ])
  worst <- names(which.max(loss))
  pos <- unname(rank(-X[, worst], ties.method = "min")[state])
  sprintf("Where it falls furthest short for you: %s (relative score %.2f, %s of %d). %s carries %.0f%% of your weight, so this is its biggest gap from an ideal place.",
          worst, X[state, worst], ordinal(pos), nrow(X), worst, 100 * w[worst])
}
