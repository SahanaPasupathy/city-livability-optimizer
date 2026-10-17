# ==============================================================================
# What-if scenarios: change one or more of the seven ratings and compare the
# resulting ranking with the baseline. Both rankings come from the same engine
# (compute_recommendation(): AHP + TOPSIS) with the same low-confidence setting,
# so the only difference between them is the changed ratings.
# ==============================================================================

#' Run a what-if scenario.
#'
#' @param answers named 1-5 baseline ratings (all seven criteria).
#' @param changes named integer ratings to use instead, for example
#'   c(Safety = 5). Names must be criteria; values whole numbers 1-5.
#' @param X decision matrix from criteria_matrix().
#' @param low_conf names of low-confidence States/UTs.
#' @param exclude_low leave them out of both rankings?
#' @return list(answers = baseline ratings, scenario_answers, changed = names of
#'   the criteria whose rating differs, base = baseline recommendation,
#'   scenario = scenario recommendation, changes = rank_change() table ordered
#'   by scenario rank, base_top, scenario_top, new_top = TRUE if #1 changed).
run_scenario <- function(answers, changes, X, low_conf = character(0), exclude_low = FALSE) {
  if (length(answers) != length(CRITERIA) || is.null(names(answers)) || !setequal(names(answers), CRITERIA) ||
      anyNA(answers)) {
    stop("All seven baseline answers are needed, named by criterion.", call. = FALSE)
  }
  if (length(changes) == 0 || is.null(names(changes)) || !all(names(changes) %in% CRITERIA) ||
      anyDuplicated(names(changes))) {
    stop("Changes must be named by criterion (for example c(Safety = 5)).", call. = FALSE)
  }
  v <- suppressWarnings(as.numeric(changes))
  if (anyNA(v) || any(v != round(v)) || any(v < 1) || any(v > 5)) {
    stop("Each changed rating must be a whole number from 1 to 5.", call. = FALSE)
  }
  answers <- setNames(as.integer(answers[CRITERIA]), CRITERIA)
  scen <- answers
  scen[names(changes)] <- as.integer(v)
  base <- compute_recommendation(answers, X, low_conf, exclude_low)
  sc <- compute_recommendation(scen, X, low_conf, exclude_low)
  ch <- rank_change(base$topsis, sc$topsis)
  base_top <- top_k(base$topsis, 1)$State
  sc_top <- top_k(sc$topsis, 1)$State
  list(answers = answers, scenario_answers = scen,
       changed = CRITERIA[scen != answers],
       base = base, scenario = sc, changes = ch,
       base_top = base_top, scenario_top = sc_top, new_top = !identical(base_top, sc_top))
}
