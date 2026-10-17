# ==============================================================================
# Pareto efficiency (used by the Trade-offs tab, the details in States & UTs and the report)
# ------------------------------------------------------------------------------
# A State/UT is DOMINATED when another State/UT is at least as good on every
# evaluated criterion and strictly better on at least one. It is PARETO-EFFICIENT
# when nothing dominates it. Efficiency says "no other place beats it on these
# criteria without losing somewhere"; it is not a ranking and ignores weights,
# which is why it is shown next to, not instead of, the TOPSIS rank.
# ==============================================================================

#' Which States/UTs are Pareto-efficient on the chosen criteria?
#'
#' Rules made explicit:
#'  * Direction: `higher_better` says, per criterion, whether a higher value is
#'    better. The project's criterion scores are all higher = better.
#'  * Ties: equal values count as "at least as good", so two States/UTs with
#'    identical scores do not dominate each other and can both be efficient.
#'    `tol` treats differences smaller than it as ties (default 0: exact).
#'  * Missing values: a State/UT with a missing value on any evaluated criterion
#'    is NOT classified (Status "insufficient data", Efficient NA) and is never
#'    used to dominate others. Nothing is imputed.
#'
#' @param X numeric matrix, States/UTs (named rows) x criteria (named columns).
#' @param criteria columns to evaluate (at least one).
#' @param higher_better logical, one per criterion (recycled if length 1).
#' @param tol differences of at most `tol` count as ties.
#' @return data.frame(State, Efficient, Status, N_Dominating) with attribute
#'   "dominated_by" (named list: State -> names of the States/UTs dominating it)
#'   and attribute "criteria".
pareto_front <- function(X, criteria = colnames(X), higher_better = TRUE, tol = 0) {
  if (!is.matrix(X) || !is.numeric(X) || is.null(rownames(X))) {
    stop("X must be a numeric matrix with State names as row names.", call. = FALSE)
  }
  if (length(criteria) < 1 || !all(criteria %in% colnames(X))) {
    stop("Unknown criteria: ", paste(setdiff(criteria, colnames(X)), collapse = ", "), call. = FALSE)
  }
  higher_better <- rep_len(as.logical(higher_better), length(criteria))
  if (anyNA(higher_better)) stop("higher_better must be TRUE or FALSE.", call. = FALSE)
  M <- X[, criteria, drop = FALSE]
  M[, !higher_better] <- -M[, !higher_better]          # now higher is better everywhere
  complete <- stats::complete.cases(M)
  states <- rownames(M)
  dominated_by <- setNames(vector("list", nrow(M)), states)
  for (i in seq_len(nrow(M))) {
    if (!complete[i]) { dominated_by[[i]] <- character(0); next }
    others <- which(complete & seq_len(nrow(M)) != i)
    ge <- apply(M[others, , drop = FALSE], 1, function(r) all(r >= M[i, ] - tol))
    gt <- apply(M[others, , drop = FALSE], 1, function(r) any(r > M[i, ] + tol))
    dominated_by[[i]] <- states[others[ge & gt]]
  }
  n_dom <- lengths(dominated_by)
  out <- data.frame(
    State        = states,
    Efficient    = ifelse(complete, n_dom == 0, NA),
    Status       = ifelse(!complete, "insufficient data", ifelse(n_dom == 0, "efficient", "dominated")),
    N_Dominating = ifelse(complete, n_dom, NA_integer_),
    row.names = NULL, stringsAsFactors = FALSE)
  attr(out, "dominated_by") <- dominated_by
  attr(out, "criteria") <- criteria
  out
}

#' Efficient States/UTs in the order they lie along a two-criterion frontier
#' (by the first criterion, ascending), for drawing the frontier line.
#' @param X matrix of scores; @param front result of pareto_front() on 2 criteria.
pareto_frontier_order <- function(X, front) {
  cr <- attr(front, "criteria")
  eff <- front$State[which(front$Efficient)]
  if (length(cr) != 2 || length(eff) == 0) return(eff)
  eff[order(X[eff, cr[1]], -X[eff, cr[2]])]
}

#' One plain-language sentence on a State/UT's Pareto status.
#' @param front result of pareto_front(); @param state a State/UT name.
#' @param max_names how many dominating States/UTs to name.
explain_pareto <- function(front, state, max_names = 3) {
  if (!state %in% front$State) stop("Unknown State/UT: ", state, call. = FALSE)
  cr <- attr(front, "criteria")
  on <- if (length(cr) <= 2) paste(cr, collapse = " and ") else sprintf("all %d criteria", length(cr))
  row <- front[front$State == state, ]
  if (row$Status == "insufficient data") {
    return(sprintf("%s cannot be assessed on %s because a score is missing.", state, on))
  }
  if (isTRUE(row$Efficient)) {
    return(sprintf("Pareto-efficient on %s: no other State/UT is at least as good on %s and better on one.",
                   on, if (length(cr) == 1) "it" else if (length(cr) == 2) "both" else "every one"))
  }
  doms <- attr(front, "dominated_by")[[state]]
  shown <- head(doms, max_names)
  more <- length(doms) - length(shown)
  sprintf("Dominated on %s: %s %s at least as good on %s and better on at least one.",
          on, paste(shown, collapse = ", "),
          if (more > 0) sprintf("and %d more are", more) else if (length(shown) == 1) "is" else "are",
          if (length(cr) == 2) "both" else "each")
}
