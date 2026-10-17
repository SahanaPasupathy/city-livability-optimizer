# ==============================================================================
# App logic that does NOT depend on Shiny, so it can be tested in a plain script
# (analysis/08_app_check.R). The Shiny app (app/app.R) only handles screens and
# clicks and calls these functions for every calculation.
# Needs common.R, ahp.R and topsis.R to be loaded first.
# ==============================================================================

# The seven flashcard questions. Icons are HTML entities so the file stays plain
# ASCII (emoji typed directly can be garbled on some Windows setups).
QUESTIONS <- data.frame(
  Criterion = CRITERIA,
  Icon = c("&#127891;", "&#128737;&#65039;", "&#127973;", "&#127968;",
           "&#128188;", "&#127807;", "&#128646;"),
  Hint = c("School learning levels and higher-secondary enrolment",
           "Low crime and low murder rates",
           "Immunisation, safe institutional deliveries and health workers",
           "Pucca houses, clean cooking fuel and piped water",
           "Low unemployment, low poverty and stable prices",
           "Lower air pollution (PM2.5)",
           "Villages with 3G/4G coverage and electric-vehicle adoption"),
  stringsAsFactors = FALSE)

# Labels for the 1-5 answer buttons.
ANSWER_LABELS <- c("Not important", "Slightly", "Moderately", "Very", "Extremely")

# Answers that give every criterion the same weight (1/7 each).
EQUAL_ANSWERS <- setNames(rep(3L, length(CRITERIA)), CRITERIA)

#' Turn the seven answers into a recommendation.
#'
#' @param answers named integer vector (1-5), one entry per criterion, no NA.
#' @param X decision matrix from criteria_matrix() (all States/UTs).
#' @param low_conf names of low-confidence States/UTs.
#' @param exclude_low if TRUE, the low-confidence States/UTs are left out of the
#'   ranking (TOPSIS is then run on the remaining States/UTs only).
#' @return list(ahp, topsis, X, excluded): the AHP result, the TOPSIS result, the
#'   matrix actually ranked and the names of excluded States/UTs.
compute_recommendation <- function(answers, X, low_conf = character(0),
                                   exclude_low = FALSE) {
  if (length(answers) != length(CRITERIA) || anyNA(answers)) {
    stop("All seven answers are needed before a recommendation can be made.",
         call. = FALSE)
  }
  ahp  <- run_ahp(answers)
  keep <- if (isTRUE(exclude_low)) !(rownames(X) %in% low_conf) else rep(TRUE, nrow(X))
  Xuse <- X[keep, , drop = FALSE]
  list(ahp = ahp, topsis = topsis(Xuse, ahp$weights), X = Xuse,
       excluded = rownames(X)[!keep])
}

#' The user's weights as a table, largest first.
#' @param ahp list returned by run_ahp().
#' @return data.frame(Criterion, Weight).
weights_table <- function(ahp) {
  w <- ahp$weights
  out <- data.frame(Criterion = names(w), Weight = unname(w), stringsAsFactors = FALSE)
  # Rounded so equal ratings keep the criterion order (see by_weight() in explain.R).
  out[order(-round(out$Weight, 12), match(out$Criterion, CRITERIA)), ]
}

#' Plain-language reading of the consistency ratio.
#' @param ahp list returned by run_ahp().
#' @return one sentence.
consistency_message <- function(ahp) {
  if (ahp$consistent) {
    sprintf("Consistency ratio %.3f (it must be 0.10 or below): your answers are consistent.", ahp$CR)
  } else {
    sprintf("Consistency ratio %.3f is above 0.10: some answers contradict each other, so treat the weights with care.", ahp$CR)
  }
}