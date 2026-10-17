# ==============================================================================
# Data quality, provenance and the reproducible score pipeline
# ------------------------------------------------------------------------------
# The app ranks with the criterion scores stored in the dataset (*_norm). The
# script that originally built them is not in the repository, so this file
# rebuilds them from the 17 raw indicators with the rule found by
# tools/audit_data.py, and records for every cell whether the value was
# observed, missing, capped or imputed. Nothing here changes the stored data;
# tests check that the rebuilt scores equal the stored ones.
#
# Rule (reproduces all 252 stored scores to 4 decimals):
#   1. indicators whose name ends in "_Pct" are capped at 100;
#   2. each indicator is capped at its 5th and 95th percentiles (R quantile
#      type 7) and min-max scaled to 0-1, reversed when lower is better;
#   3. a criterion score is the mean of the indicators that are present
#      (missing indicators are skipped, not penalised);
#   4. the criterion is rescaled to 0-1 across the States/UTs that have one;
#   5. a criterion with no indicator at all gets the median of the other
#      States/UTs' scores (only Lakshadweep's Connectivity) and is flagged
#      as IMPUTED, never as measured.
# Needs common.R; uses load_indicator_metadata() from catalogue.R for `meta`.
# ==============================================================================

WINSOR_P <- 0.05

# Plain-language names and honest scope for each criterion. The internal names
# in CRITERIA stay unchanged (they are column names); interfaces should show
# `Label` and, where space allows, `Measures` and `Not_Measured`.
CRITERION_INFO <- data.frame(
  Criterion = CRITERIA,
  Label = c("Education (school learning and enrolment)",
            "Safety (recorded crime)",
            "Health (health-service coverage)",
            "Housing (basic household amenities)",
            "Economy (unemployment, poverty and inflation)",
            "Environment (air quality, PM2.5)",
            "Connectivity (village mobile coverage and EV adoption)"),
  Measures = c("An education index, Grade 8 learning levels and higher-secondary enrolment.",
               "Recorded crime rate and recorded murders per lakh people.",
               "Immunisation coverage, births in health facilities and health-worker density.",
               "Share of kachha (non-permanent) houses, clean cooking fuel and piped water in rural homes.",
               "Unemployment rate, multidimensional poverty and average inflation.",
               "Fine-particle air pollution (PM2.5) only.",
               "Villages with 3G/4G mobile coverage and electric vehicles per lakh people."),
  Not_Measured = c("School or college quality near a particular home.",
                   "Personal safety. Recorded crime also depends on how often crimes are reported.",
                   "Distance to hospitals, specialist care or the cost of treatment.",
                   "Rent, house prices, affordability or availability of housing.",
                   "Salaries, job openings in a particular field or the overall cost of living.",
                   "Other pollution, water quality, noise, green space or climate.",
                   "Public transport, roads, commute times, airports or home broadband."),
  stringsAsFactors = FALSE)

#' Public label for internal criterion names.
criterion_label <- function(criterion) {
  CRITERION_INFO$Label[match(criterion, CRITERION_INFO$Criterion)]
}

#' Cap and scale one indicator (steps 1-2 of the rule).
#' @param x numeric vector (NA allowed).
#' @param direction "higher_better" or "lower_better".
#' @param cap_pct cap at 100 first?
#' @param p percentile for the caps (0 turns capping off).
#' @return list(scaled = 0-1 values with higher = better, status = character
#'   vector: "missing", "observed", "capped at 100", "capped at the 5th
#'   percentile" or "capped at the 95th percentile").
scale_indicator <- function(x, direction, cap_pct = FALSE, p = WINSOR_P) {
  status <- ifelse(is.na(x), "missing", "observed")
  v <- x
  if (cap_pct) {
    over <- !is.na(v) & v > 100
    status[over] <- "capped at 100"
    v[over] <- 100
  }
  q <- stats::quantile(v, c(p, 1 - p), na.rm = TRUE, names = FALSE, type = 7)
  lo <- q[1]; hi <- q[2]
  status[!is.na(v) & v < lo & status == "observed"] <- "capped at the 5th percentile"
  status[!is.na(v) & v > hi & status == "observed"] <- "capped at the 95th percentile"
  v <- pmin(pmax(v, lo), hi)
  s <- if (hi == lo) ifelse(is.na(v), NA_real_, 0) else (v - lo) / (hi - lo)
  if (direction == "lower_better") s <- 1 - s
  list(scaled = s, status = status)
}

#' Rebuild the seven criterion scores from the raw indicators.
#' @param d data.frame from load_livability_data().
#' @param meta data.frame from load_indicator_metadata().
#' @param p percentile for the caps.
#' @return list(scores = 36 x 7 matrix as stored, used = matrix of indicator
#'   counts per cell, total = indicators per criterion, imputed = logical
#'   matrix, scaled = States x indicators matrix of 0-1 values,
#'   status = States x indicators matrix of cell status).
build_criterion_scores <- function(d, meta, p = WINSOR_P) {
  if (!all(INDICATORS %in% names(d))) stop("The dataset lacks raw indicator columns.", call. = FALSE)
  meta <- meta[match(INDICATORS, meta$indicator), ]
  n <- nrow(d)
  scaled <- matrix(NA_real_, n, length(INDICATORS), dimnames = list(d$State, INDICATORS))
  status <- matrix(NA_character_, n, length(INDICATORS), dimnames = list(d$State, INDICATORS))
  for (j in seq_along(INDICATORS)) {
    ind <- INDICATORS[j]
    s <- scale_indicator(d[[ind]], meta$direction[j], cap_pct = endsWith(ind, "_Pct"), p = p)
    scaled[, j] <- s$scaled
    status[, j] <- s$status
  }
  scores <- matrix(NA_real_, n, length(CRITERIA), dimnames = list(d$State, CRITERIA))
  used <- matrix(0L, n, length(CRITERIA), dimnames = list(d$State, CRITERIA))
  imputed <- matrix(FALSE, n, length(CRITERIA), dimnames = list(d$State, CRITERIA))
  total <- setNames(integer(length(CRITERIA)), CRITERIA)
  for (cn in CRITERIA) {
    inds <- meta$indicator[meta$criterion == cn]
    total[cn] <- length(inds)
    block <- scaled[, inds, drop = FALSE]
    used[, cn] <- rowSums(!is.na(block))
    comp <- ifelse(used[, cn] > 0, rowMeans(block, na.rm = TRUE), NA_real_)
    rng <- range(comp, na.rm = TRUE)
    sc <- if (diff(rng) == 0) ifelse(is.na(comp), NA_real_, 0) else (comp - rng[1]) / diff(rng)
    none <- is.na(sc)
    if (any(none)) {
      sc[none] <- stats::median(sc[!none])
      imputed[none, cn] <- TRUE
    }
    scores[, cn] <- sc
  }
  list(scores = scores, used = used, total = total, imputed = imputed,
       scaled = scaled, status = status)
}

#' Coverage of every criterion score: how many of its indicators were available.
#'
#' Support levels (documented in docs/DATA_AUDIT.md and the methodology):
#'   "full"     every indicator of the criterion is available;
#'   "partial"  at least half, but not all, are available;
#'   "thin"     fewer than half are available (but at least one);
#'   "imputed"  none is available; the stored score was filled in.
#' @return data.frame(State, Criterion, Available, Total, Support, Score).
coverage_table <- function(d, meta) {
  b <- build_criterion_scores(d, meta)
  X <- criteria_matrix(d)
  out <- expand.grid(State = d$State, Criterion = CRITERIA, stringsAsFactors = FALSE)
  out$Available <- b$used[cbind(out$State, out$Criterion)]
  out$Total <- unname(b$total[out$Criterion])
  share <- out$Available / out$Total
  out$Support <- ifelse(out$Available == 0, "imputed",
                        ifelse(share == 1, "full", ifelse(share >= 0.5, "partial", "thin")))
  out$Score <- X[cbind(out$State, out$Criterion)]
  out[order(match(out$State, d$State), match(out$Criterion, CRITERIA)), ]
}

#' Place-level coverage summary and the "limited data coverage" flag.
#'
#' A State/UT has LIMITED COVERAGE when 3 or more of its 17 indicators are
#' missing, OR any criterion is "thin" or "imputed". (On the current data this
#' flags the same four places as the stored Low_Confidence column: A&N Islands,
#' Chandigarh, Ladakh, Lakshadweep.) Places with "partial" criteria are not
#' flagged but get a note.
#' @return data.frame(State, Missing_Indicators, Thin, Partial, Imputed,
#'   Limited_Coverage, Note), one row per State/UT in dataset order.
coverage_summary <- function(d, meta) {
  ct <- coverage_table(d, meta)
  missing <- rowSums(is.na(d[INDICATORS]))
  pick <- function(st, lev) {
    x <- ct$Criterion[ct$State == st & ct$Support %in% lev]
    if (length(x)) paste(x, collapse = ", ") else ""
  }
  out <- data.frame(State = d$State, Missing_Indicators = unname(missing),
                    Thin = vapply(d$State, pick, "", lev = "thin", USE.NAMES = FALSE),
                    Partial = vapply(d$State, pick, "", lev = "partial", USE.NAMES = FALSE),
                    Imputed = vapply(d$State, pick, "", lev = "imputed", USE.NAMES = FALSE),
                    stringsAsFactors = FALSE)
  out$Limited_Coverage <- out$Missing_Indicators >= 3 | nzchar(out$Thin) | nzchar(out$Imputed)
  out$Note <- vapply(seq_len(nrow(out)), function(i) coverage_note(out[i, ]), "")
  out
}

# One plain sentence from a coverage_summary() row ("" when complete).
coverage_note <- function(r) {
  parts <- c(
    if (r$Missing_Indicators > 0) sprintf("%d of 17 indicators missing", r$Missing_Indicators),
    if (nzchar(r$Imputed)) sprintf("no measured data for %s (its score is an estimate: the median of the other places)", r$Imputed),
    if (nzchar(r$Thin)) sprintf("fewer than half of the usual indicators for %s", r$Thin),
    if (nzchar(r$Partial)) sprintf("only some of the usual indicators for %s", r$Partial))
  if (!length(parts)) return("")
  paste0(if (isTRUE(r$Limited_Coverage)) "Limited data coverage: " else "Data note: ",
         paste(parts, collapse = "; "), ".")
}

#' Names of the States/UTs with limited data coverage (see coverage_summary()).
limited_coverage_states <- function(d, meta) {
  cs <- coverage_summary(d, meta)
  cs$State[cs$Limited_Coverage]
}

#' Every raw indicator value of one State/UT with its unit, status and provenance.
#' @return data.frame(Indicator, Label, Criterion, Raw, Unit, Unit_Status,
#'   Direction, Status, Provenance, Scaled), in INDICATORS order. `Raw` is the
#'   value as stored in the dataset (never capped); `Scaled` is its 0-1 score.
indicator_detail <- function(d, meta, state) {
  if (!state %in% d$State) stop("Unknown State/UT: ", state, call. = FALSE)
  b <- build_criterion_scores(d, meta)
  meta <- meta[match(INDICATORS, meta$indicator), ]
  row <- d[d$State == state, ]
  label <- if ("public_label" %in% names(meta)) meta$public_label else meta$label
  data.frame(Indicator = INDICATORS, Label = label, Criterion = meta$criterion,
             Raw = vapply(INDICATORS, function(i) as.numeric(row[[i]]), numeric(1), USE.NAMES = FALSE),
             Unit = meta$unit, Unit_Status = meta$unit_status,
             Direction = ifelse(meta$direction == "higher_better", "higher is better", "lower is better"),
             Status = unname(b$status[state, INDICATORS]),
             Provenance = provenance_status(meta),
             Scaled = unname(b$scaled[state, INDICATORS]),
             row.names = NULL, stringsAsFactors = FALSE)
}

#' Provenance in one short phrase per indicator, from the metadata.
provenance_status <- function(meta) {
  if ("provenance_status" %in% names(meta)) return(meta$provenance_status)
  ifelse(startsWith(meta$source_traceable, "no"), "source not traced", "source not recorded")
}
