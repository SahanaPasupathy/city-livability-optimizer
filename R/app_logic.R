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
  # Hints say what is actually measured and what is not (CRITERION_INFO in
  # R/data_quality.R holds the same facts for other screens).
  Hint = c("Measured by an education index, Grade 8 learning levels and higher-secondary enrolment. Not school quality near a particular home.",
           "Measured by recorded crime and recorded murders. Not personal safety; recorded crime also depends on reporting.",
           "Measured by immunisation, births in health facilities and health-worker numbers. Not distance to hospitals.",
           "Measured by permanent (pucca) houses, clean cooking fuel and rural piped water. Not rent or house prices.",
           "Measured by unemployment, poverty and average inflation. Not salaries, job openings or the cost of living.",
           "Measured by fine-particle air pollution (PM2.5) only.",
           "Measured by village 3G/4G coverage and electric-vehicle adoption. Not public transport or commute times."),
  stringsAsFactors = FALSE)
# Question titles use the honest labels (criterion name plus what it measures).
QUESTIONS$Label <- c("Education (school learning and enrolment)",
                     "Safety (recorded crime)",
                     "Health (health-service coverage)",
                     "Housing (basic household amenities)",
                     "Economy (unemployment, poverty and inflation)",
                     "Environment (air quality)",
                     "Connectivity (village mobile coverage and EVs)")

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

#' Plain-language note on the weights.
#'
#' The AHP consistency ratio is still computed (ahp$CR) but is not shown as a
#' quality signal: with 1-5 ratings turned into comparisons by the difference
#' rule it is always 0.10 or below (tested in tests/testthat/test-ahp.R), so it
#' cannot tell the user anything. The message instead says what the weights mean.
#' @param ahp list returned by run_ahp().
#' @return one sentence.
consistency_message <- function(ahp) {
  w <- ahp$weights
  ratio <- max(w) / min(w)
  if (ratio < 1.05) {
    return("Your weights come straight from your 1-5 answers. You rated everything the same, so every criterion counts equally.")
  }
  sprintf("Your weights come straight from your 1-5 answers: gaps between answers are magnified, so your top criterion (%s) counts about %s times as much as your lowest (%s).",
          names(w)[which.max(w)], format(round(ratio, if (ratio < 10) 1 else 0), nsmall = 0), names(w)[which.min(w)])
}

#' The user's priorities in plain words (for "What matters most to you").
#' @param ahp list returned by run_ahp().
#' @return data.frame(Criterion, Label, Rating, Weight, Percent, Sentence),
#'   largest weight first; Percent is a rounded text such as "32%".
weights_view <- function(ahp) {
  wt <- weights_table(ahp)
  a <- ahp$answers
  if (is.null(names(a))) names(a) <- CRITERIA
  data.frame(Criterion = wt$Criterion,
             Label = criterion_label(wt$Criterion),
             Rating = unname(as.integer(a[wt$Criterion])),
             Weight = wt$Weight,
             Percent = sprintf("%.0f%%", 100 * wt$Weight),
             Sentence = sprintf("%s counts for about %.0f%% of your result.", wt$Criterion, 100 * wt$Weight),
             row.names = NULL, stringsAsFactors = FALSE)
}
