# ==============================================================================
# Top-k recommendation and a dynamically generated explanation.
# Every sentence is built from the numbers of THIS user's run (their weights,
# the State/UT's scores, its TOPSIS distances), so the text changes whenever
# the answers change. Nothing is hard-coded per State/UT or per persona.
# ==============================================================================

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
#'  * "strong"          = the State/UT is in the top third (rank <= 12 of 36)
#'                        on that criterion; "weak" = bottom third (rank >= 25).
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
  fmt_item <- function(cn) sprintf("%s (score %.2f, %s of %d)",
                                   cn, score[cn], ordinal(pos[cn]), n)
  
  important <- colnames(X)[w > 1 / p]
  important <- important[order(-w[important])]
  strong    <- important[pos[important] <= ceiling(n / 3)]
  weak      <- important[pos[important] > n - ceiling(n / 3)]
  others    <- setdiff(colnames(X), important)
  bonus     <- others[pos[others] <= 5]
  
  top_w <- names(sort(w, decreasing = TRUE))[1:3]
  shortfall <- w * (1 - score)
  worst <- names(which.max(shortfall))
  
  out <- character(0)
  out <- c(out, sprintf(
    "%s ranks %s of %d for you, with a closeness of %.3f (distance to the ideal D+ = %.3f, to the worst case D- = %.3f).",
    state, ordinal(row$Rank), n, row$Closeness, row$D_plus, row$D_minus))
  out <- c(out, sprintf("What matters most to you: %s.",
                        paste(sprintf("%s (%.0f%%)", top_w, 100 * w[top_w]), collapse = ", ")))
  out <- c(out, if (length(strong) > 0)
    sprintf("Strong where it matters: %s.", paste(sapply(strong, fmt_item), collapse = "; "))
    else "It is not in the top third of States/UTs on any criterion that matters most to you.")
  if (length(weak) > 0) {
    out <- c(out, sprintf("Weaker on your priorities: %s.",
                          paste(sapply(weak, fmt_item), collapse = "; ")))
  }
  if (length(bonus) > 0) {
    out <- c(out, sprintf("Also near the top on criteria you weighted less: %s.",
                          paste(sapply(bonus, fmt_item), collapse = "; ")))
  }
  out <- c(out, sprintf(
    "Its biggest shortfall from the ideal is %s: score %.2f with %.0f%% weight costs it %.3f of a possible %.3f weighted points.",
    worst, score[worst], 100 * w[worst], shortfall[worst], w[worst]))
  if (state %in% low_confidence) {
    out <- c(out, "Caution: this State/UT is flagged low confidence (small population or thin data), so treat its position as indicative.")
  }
  out
}