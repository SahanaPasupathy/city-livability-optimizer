# ==============================================================================
# State/UT catalogue and per-State data notes (Explore view, detail panel)
# ------------------------------------------------------------------------------
# Everything here is read from the dataset and the indicator metadata; nothing
# is described that the data do not contain.
# ==============================================================================

METADATA_PATH <- file.path("data", "indicator_metadata.csv")

#' Read the indicator metadata (labels, criteria, directions, sources).
#' @param path location of data/indicator_metadata.csv.
#' @return data.frame with one row per indicator, in INDICATORS order.
load_indicator_metadata <- function(path = METADATA_PATH) {
  if (!file.exists(path)) {
    stop("Indicator metadata not found at '", path, "'.", call. = FALSE)
  }
  m <- read.csv(path, stringsAsFactors = FALSE)
  if (!setequal(m$indicator, INDICATORS)) {
    stop("data/indicator_metadata.csv does not list the 17 indicators.", call. = FALSE)
  }
  m[match(INDICATORS, m$indicator), ]
}

#' One row per State/UT for the catalogue: scores, current rank and data flags.
#'
#' @param rec list from compute_recommendation() (the ranking currently shown).
#' @param d data.frame from load_livability_data() (all 36 States/UTs).
#' @return data.frame(State, Type, Region, Rank, Closeness, Low_Confidence,
#'   Missing_Indicators, <seven criterion scores>), ordered by Rank then name.
#'   States/UTs left out of the current ranking have Rank and Closeness NA.
catalogue_data <- function(rec, d) {
  res <- rec$topsis$results
  i <- match(d$State, res$State)
  out <- data.frame(State = d$State, Type = d$Type, Region = d$Region,
                    Rank = res$Rank[i], Closeness = res$Closeness[i],
                    Low_Confidence = d$Low_Confidence == 1,
                    Missing_Indicators = d$Missing_Subindicators,
                    stringsAsFactors = FALSE)
  scores <- criteria_matrix(d)
  out <- cbind(out, as.data.frame(scores[d$State, , drop = FALSE], row.names = NULL))
  out[order(is.na(out$Rank), out$Rank, out$State), ]
}

#' Data-quality notes for one State/UT, from the dataset itself.
#'
#' @param d data.frame from load_livability_data().
#' @param meta data.frame from load_indicator_metadata().
#' @param state a State/UT name.
#' @return list(missing = data.frame(Indicator, Label, Criterion),
#'   criteria_without_data = criteria where every indicator is missing,
#'   partial = criteria scored on fewer than all of their indicators,
#'   low_confidence = TRUE/FALSE).
state_data_notes <- function(d, meta, state) {
  if (!state %in% d$State) stop("Unknown State/UT: ", state, call. = FALSE)
  row <- d[d$State == state, ]
  is_na <- vapply(meta$indicator, function(ind) is.na(row[[ind]]), logical(1))
  missing <- meta[is_na, c("indicator", "label", "criterion")]
  names(missing) <- c("Indicator", "Label", "Criterion")
  per_crit <- table(factor(meta$criterion, levels = CRITERIA))
  miss_crit <- table(factor(missing$Criterion, levels = CRITERIA))
  list(missing = missing,
       criteria_without_data = CRITERIA[miss_crit == per_crit & per_crit > 0],
       partial = CRITERIA[miss_crit > 0 & miss_crit < per_crit],
       low_confidence = isTRUE(row$Low_Confidence == 1))
}
