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

#' Plain summary of a scenario: what changed in the answers, the weights and
#' the shortlist. This is the contract for "change my preferences and see what
#' changes": the edited answers are the scenario, the previous answers the
#' baseline, and both are ranked by compute_recommendation() with the same
#' low-confidence setting (via run_scenario()).
#'
#' @param answers named 1-5 baseline (previous) answers.
#' @param changes named new ratings for one or more criteria (several allowed),
#'   or a complete set of seven new answers.
#' @param X,low_conf,exclude_low as in run_scenario().
#' @param k size of the shortlist compared (default 3).
#' @return list(changed = data.frame(Criterion, From, To, Weight_Before,
#'   Weight_After), top_before, top_after, entered, left, new_top,
#'   moves = data.frame(State, Before, After, Change) for every place in either
#'   shortlist, ordered by new rank, sentences = character vector,
#'   scenario = the run_scenario() result).
scenario_view <- function(answers, changes, X, low_conf = character(0), exclude_low = FALSE, k = 3) {
  sc <- run_scenario(answers, changes, X, low_conf, exclude_low)
  wb <- sc$base$ahp$weights; wa <- sc$scenario$ahp$weights
  changed <- data.frame(Criterion = sc$changed,
                        From = unname(sc$answers[sc$changed]), To = unname(sc$scenario_answers[sc$changed]),
                        Weight_Before = unname(wb[sc$changed]), Weight_After = unname(wa[sc$changed]),
                        stringsAsFactors = FALSE)
  tb <- top_k(sc$base$topsis, k)$State
  ta <- top_k(sc$scenario$topsis, k)$State
  ch <- sc$changes
  show <- ch[ch$State %in% union(tb, ta), ]
  moves <- data.frame(State = show$State, Before = show$Baseline_Rank, After = show$Personal_Rank,
                      Change = show$Rank_Change, stringsAsFactors = FALSE)
  moves <- moves[order(moves$After, moves$State), ]
  rownames(moves) <- NULL
  entered <- setdiff(ta, tb); left <- setdiff(tb, ta)
  s <- character(0)
  if (nrow(changed) == 0) {
    s <- "Your answers are the same as before, so nothing changed."
  } else {
    s <- c(s, paste0("You changed: ", paste(sprintf("%s from %d to %d (weight %.0f%% to %.0f%%)", changed$Criterion,
                                                     changed$From, changed$To, 100 * changed$Weight_Before,
                                                     100 * changed$Weight_After), collapse = "; "), "."))
    s <- c(s, if (sc$new_top) sprintf("Your #1 changed from %s to %s.", sc$base_top, sc$scenario_top)
              else sprintf("Your #1 is still %s.", sc$base_top))
    if (length(entered)) s <- c(s, sprintf("New in your top %d: %s (replacing %s).", k,
                                           paste(entered, collapse = ", "), paste(left, collapse = ", ")))
    else if (!identical(ta, tb)) s <- c(s, sprintf("Same top %d, in a different order.", k))
    else s <- c(s, sprintf("Your top %d is unchanged.", k))
  }
  list(changed = changed, top_before = tb, top_after = ta, entered = entered, left = left,
       new_top = sc$new_top, moves = moves, sentences = s, scenario = sc)
}

#' Stability of the shortlist under small changes to the user's own answers.
#'
#' Each answer is moved one point up and one point down, ONE AT A TIME (moves
#' outside 1-5 are skipped), giving up to 14 variants. Every variant is ranked
#' with compute_recommendation() on the same places. The result is a COUNT of
#' tested variants, never a probability, and it says nothing about data
#' accuracy. It is deterministic: no random numbers are used.
#'
#' @param answers named 1-5 answers (all seven criteria).
#' @param X,low_conf,exclude_low as in compute_recommendation().
#' @param k shortlist size.
#' @return list(n_variants, top1 = baseline #1, top1_kept = variants where it
#'   stays #1, shortlist = data.frame(State, Rank, Stayed_Top_k, N_Variants),
#'   close_call = TRUE if any variant changes the order or membership of the
#'   top k, also_consider = places outside the top k that enter it in at least
#'   one variant (data.frame(State, Rank, Entered_Top_k), most often first),
#'   variants = data.frame(Criterion, To, Top1, Top_k), sentences, notes).
stability_check <- function(answers, X, low_conf = character(0), exclude_low = FALSE, k = 3) {
  if (length(answers) != length(CRITERIA) || is.null(names(answers)) || !setequal(names(answers), CRITERIA) ||
      anyNA(answers)) {
    stop("All seven answers are needed, named by criterion.", call. = FALSE)
  }
  answers <- setNames(as.integer(answers[CRITERIA]), CRITERIA)
  base <- compute_recommendation(answers, X, low_conf, exclude_low)
  bt <- top_k(base$topsis, k)$State
  grid <- expand.grid(Criterion = CRITERIA, Step = c(-1L, 1L), stringsAsFactors = FALSE)
  grid$To <- answers[grid$Criterion] + grid$Step
  grid <- grid[grid$To >= 1 & grid$To <= 5, ]
  grid <- grid[order(match(grid$Criterion, CRITERIA), grid$Step), ]
  tops <- lapply(seq_len(nrow(grid)), function(i) {
    a <- answers; a[grid$Criterion[i]] <- grid$To[i]
    top_k(compute_recommendation(a, X, low_conf, exclude_low)$topsis, k)$State
  })
  nv <- length(tops)
  stayed <- vapply(bt, function(s) sum(vapply(tops, function(t) s %in% t, logical(1))), integer(1))
  top1_kept <- sum(vapply(tops, function(t) identical(t[1], bt[1]), logical(1)))
  others <- table(unlist(lapply(tops, function(t) setdiff(t, bt))))
  res <- base$topsis$results
  also <- data.frame(State = names(others), Entered_Top_k = as.integer(others), stringsAsFactors = FALSE)
  also$Rank <- res$Rank[match(also$State, res$State)]
  also <- also[order(-also$Entered_Top_k, also$Rank), c("State", "Rank", "Entered_Top_k")]
  rownames(also) <- NULL
  close_call <- any(vapply(tops, function(t) !identical(t, bt), logical(1)))
  shortlist <- data.frame(State = bt, Rank = res$Rank[match(bt, res$State)], Stayed_Top_k = unname(stayed),
                          N_Variants = nv, stringsAsFactors = FALSE)
  s <- c(sprintf("Your #1, %s, stayed first in %d of %d small changes to your answers.", bt[1], top1_kept, nv),
         sprintf("%s stayed in your top %d in %d of %d.", bt, k, stayed, nv))
  s <- c(s, if (close_call) sprintf("This is a close call: small changes to your answers reorder or replace places in your top %d.", k)
            else sprintf("Small changes to your answers did not change your top %d.", k))
  if (nrow(also)) s <- c(s, sprintf("Also worth comparing: %s.",
                                    paste(sprintf("%s (entered your top %d in %d of %d)", also$State, k, also$Entered_Top_k, nv),
                                          collapse = ", ")))
  variants <- data.frame(Criterion = grid$Criterion, To = grid$To,
                         Top1 = vapply(tops, `[`, "", 1),
                         Top_k = vapply(tops, paste, "", collapse = ", "), row.names = NULL, stringsAsFactors = FALSE)
  list(n_variants = nv, top1 = bt[1], top1_kept = top1_kept, shortlist = shortlist, close_call = close_call,
       also_consider = also, variants = variants, sentences = s, notes = STABILITY_NOTES)
}
