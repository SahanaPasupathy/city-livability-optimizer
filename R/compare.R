# ==============================================================================
# Compare a personalised TOPSIS ranking with the equal-weight baseline.
# "Baseline" = the same TOPSIS run with all seven weights equal (1/7 each), so
# the ONLY difference between the two rankings is the user's weights.
# ==============================================================================

#' How far does each State/UT move when the user's weights replace equal weights?
#'
#' rank_change = baseline rank - personalised rank
#'   positive -> the State/UT moves UP for this user (e.g. +10 = ten places higher)
#'   negative -> it moves DOWN; zero -> unchanged.
#'
#' @param baseline_res list returned by topsis() with equal weights.
#' @param personal_res list returned by topsis() with the user's AHP weights.
#' @return data.frame(State, Baseline_Rank, Personal_Rank, Rank_Change,
#'   Baseline_Closeness, Personal_Closeness), ordered by Personal_Rank.
rank_change <- function(baseline_res, personal_res) {
  b <- baseline_res$results
  p <- personal_res$results
  if (!setequal(b$State, p$State)) {
    stop("Both rankings must cover the same States/UTs.", call. = FALSE)
  }
  p <- p[match(b$State, p$State), ]
  out <- data.frame(
    State              = b$State,
    Baseline_Rank      = b$Rank,
    Personal_Rank      = p$Rank,
    Rank_Change        = b$Rank - p$Rank,
    Baseline_Closeness = b$Closeness,
    Personal_Closeness = p$Closeness,
    stringsAsFactors = FALSE
  )
  out[order(out$Personal_Rank), ]
}

#' The States/UTs that gain or lose the most places.
#' @param changes data.frame returned by rank_change().
#' @param n how many to return in each direction.
#' @return list(up = biggest gainers, down = biggest losers).
top_movers <- function(changes, n = 5) {
  list(
    up   = head(changes[order(-changes$Rank_Change, changes$Personal_Rank), ], n),
    down = head(changes[order(changes$Rank_Change, changes$Personal_Rank), ], n)
  )
}

#' How many of the baseline's top-k States/UTs are still in the personalised top-k?
#' @param changes data.frame returned by rank_change().
#' @param k size of the top group.
#' @return integer between 0 and k.
top_k_overlap <- function(changes, k = 3) {
  sum(changes$Baseline_Rank <= k & changes$Personal_Rank <= k)
}