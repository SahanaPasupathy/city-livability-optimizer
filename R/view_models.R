# ==============================================================================
# View models: stable, Shiny-free contracts for the interface
# ------------------------------------------------------------------------------
# Each function returns plain data (data.frames, lists, character vectors) and
# never HTML, so the interface can present it in any design. All numbers come
# from the existing engine (compute_recommendation(), topsis(), pareto_front(),
# similar_states(), run_scenario(), robustness_analysis()) and the data-quality
# functions in R/data_quality.R. Every function works on the SAME set of ranked
# places as the recommendation it is given (`rec$X`), so the included/excluded
# set agrees across the shortlist, comparison, details, stability and report.
# Contracts and examples: docs/ARCHITECTURE.md, section "View-model contracts".
# Needs: common, ahp, topsis, similarity, explain, compare, app_logic, app_text,
# pareto, catalogue, data_quality, scenario, robustness, sensitivity.
# ==============================================================================

# Shown wherever a relative match score appears.
MATCH_NOTE <- "The relative match (0-1) compares a place with the best and worst of the places ranked for your priorities. It is not a livability percentage and changes if places are left out."
SCOPE_NOTE <- "Based on State/UT-level indicators. Conditions in particular cities and neighbourhoods can differ a lot."

# Unit text for display: only what the metadata establishes.
unit_display <- function(unit, unit_status) {
  ifelse(unit_status == "not documented", "unit not documented", unit)
}

# "79.5 percent" style value text; "no data" for missing values.
value_text <- function(raw, unit, unit_status) {
  num <- ifelse(is.na(raw), "", trimws(formatC(raw, format = "fg", digits = 4)))
  ifelse(is.na(raw), "no data",
         ifelse(unit_status == "not documented", paste0(num, " (unit not documented)"), paste(num, unit)))
}

# Indicator evidence for one place, with display fields.
indicator_evidence <- function(d, meta, state) {
  ev <- indicator_detail(d, meta, state)
  ev$Unit_Display <- unit_display(ev$Unit, ev$Unit_Status)
  ev$Value_Text <- value_text(ev$Raw, ev$Unit, ev$Unit_Status)
  ev$Evidence <- sprintf("%s: %s%s%s", ev$Label, ev$Value_Text,
                         ifelse(ev$Status %in% c("observed", "missing"), "", paste0(" (", ev$Status, ")")),
                         ifelse(ev$Provenance == "source not traced", " [source not traced]", ""))
  ev
}

# Position (1 = best) of every place in `X` on criterion `cn`.
positions <- function(X, cn) rank(-X[, cn], ties.method = "min")

#' Recommendation view model (the main result).
#'
#' @param answers named 1-5 answers (all seven criteria).
#' @param X full decision matrix (criteria_matrix(d)).
#' @param d,meta dataset and indicator metadata.
#' @param low_conf names of limited-coverage places (limited_coverage_states()).
#' @param exclude_low leave them out of the ranking?
#' @param k shortlist size.
#' @return list(
#'   rec          the compute_recommendation() result (for other view models),
#'   weights      weights_view(): Criterion, Label, Rating, Weight, Percent, Sentence,
#'   weights_note one sentence on how the answers became weights,
#'   shortlist    data.frame(Rank, State, Type, Relative_Match, Limited_Coverage,
#'                Coverage_Note, Stayed_Top_k, N_Variants),
#'   reasons      named list: State -> data.frame(Criterion, Label, Weight, Score,
#'                Position, Verdict, Support, Sentence, Evidence),
#'   compromise   named character: State -> biggest gap sentence,
#'   stability    stability_check() result (counts, close_call, also_consider),
#'   alternative_without_limited  top k without limited-coverage places when one
#'                is in the shortlist and they are not already left out, else NULL,
#'   excluded, n_ranked, notes = c(MATCH_NOTE, SCOPE_NOTE)).
recommendation_view <- function(answers, X, d, meta, low_conf = character(0), exclude_low = FALSE, k = 3) {
  rec <- compute_recommendation(answers, X, low_conf, exclude_low)
  top <- top_k(rec$topsis, k)
  w <- rec$ahp$weights
  cs <- coverage_summary(d, meta)
  ct <- coverage_table(d, meta)
  stab <- stability_check(answers, X, low_conf, exclude_low, k)
  reasons <- setNames(lapply(top$State, function(st) place_reasons(rec, d, meta, st, 3)), top$State)
  shortlist <- data.frame(
    Rank = top$Rank, State = top$State, Type = d$Type[match(top$State, d$State)],
    Relative_Match = round(top$Closeness, 2),
    Limited_Coverage = top$State %in% low_conf,
    Coverage_Note = cs$Note[match(top$State, cs$State)],
    Stayed_Top_k = stab$shortlist$Stayed_Top_k[match(top$State, stab$shortlist$State)],
    N_Variants = stab$n_variants, row.names = NULL, stringsAsFactors = FALSE)
  alt <- NULL
  if (!isTRUE(exclude_low) && any(top$State %in% low_conf)) {
    alt <- top_k(compute_recommendation(answers, X, low_conf, TRUE)$topsis, k)$State
  }
  list(rec = rec, weights = weights_view(rec$ahp), weights_note = consistency_message(rec$ahp),
       shortlist = shortlist, reasons = reasons,
       compromise = setNames(vapply(top$State, function(st) shortfall_sentence(rec$topsis, w, st), ""), top$State),
       stability = stab, alternative_without_limited = alt,
       excluded = rec$excluded, n_ranked = nrow(rec$X), notes = c(MATCH_NOTE, SCOPE_NOTE))
}

#' Reasons for one ranked place: its position on the user's k most important
#' criteria, with coverage and the indicator evidence behind each.
#' @return data.frame(Criterion, Label, Weight, Score, Position, Verdict,
#'   Support, Sentence, Evidence).
place_reasons <- function(rec, d, meta, state, k = 3) {
  pb <- priority_breakdown(rec$topsis, rec$ahp$weights, state, k)
  ev <- indicator_evidence(d, meta, state)
  ct <- coverage_table(d, meta)
  pb$Label <- criterion_label(pb$Criterion)
  pb$Support <- ct$Support[match(paste(state, pb$Criterion), paste(ct$State, ct$Criterion))]
  pb$Evidence <- vapply(pb$Criterion, function(cn) paste(ev$Evidence[ev$Criterion == cn], collapse = "; "), "")
  pb[c("Criterion", "Label", "Weight", "Score", "Position", "Verdict", "Support", "Sentence", "Evidence")]
}

#' Plain-language trade-off sentences between two places.
#'
#' Uses pareto_front() on the two places to detect a one-sided comparison
#' ("as good or better on every criterion") and otherwise lists where each is
#' stronger. Differences smaller than `similar_within` on the 0-1 scale are
#' reported as similar (a display convention, not a statistical test). The
#' last sentence names the difference that matters most for the user's weights.
#' @param S matrix of criterion scores with both places as rows.
#' @param a,b place names; @param w named weights.
#' @return character vector.
tradeoff_sentences <- function(S, a, b, w, similar_within = 0.05) {
  w <- w[colnames(S)] / sum(w)
  diff <- S[a, ] - S[b, ]
  ord <- names(sort(w, decreasing = TRUE))
  fr <- pareto_front(S[c(a, b), , drop = FALSE])
  out <- character(0)
  if (identical(attr(fr, "dominated_by")[[b]], a)) {
    out <- sprintf("On every criterion, %s scores at least as well as %s.", a, b)
  } else if (identical(attr(fr, "dominated_by")[[a]], b)) {
    out <- sprintf("On every criterion, %s scores at least as well as %s.", b, a)
  } else {
    sa <- intersect(ord, names(diff)[diff >= similar_within])
    sb <- intersect(ord, names(diff)[diff <= -similar_within])
    out <- sprintf("There is no overall winner: %s is stronger on %s; %s is stronger on %s.",
                   a, if (length(sa)) paste(sa, collapse = ", ") else "nothing by a clear margin",
                   b, if (length(sb)) paste(sb, collapse = ", ") else "nothing by a clear margin")
  }
  same <- intersect(ord, names(diff)[abs(diff) < similar_within])
  if (length(same)) out <- c(out, sprintf("They are similar on %s.", paste(same, collapse = ", ")))
  big <- names(which.max(w * abs(diff)))
  if (abs(diff[big]) >= similar_within) {
    out <- c(out, sprintf("For your priorities, the difference that matters most is %s (%.0f%% of your weight): %s is ahead.",
                          big, 100 * w[big], if (diff[big] > 0) a else b))
  }
  out
}

#' Comparison view model for one to three places.
#'
#' Places left out of the current ranking stay comparable on their scores but
#' have no rank. Positions are among the places ranked (NA for a place left out).
#' @param rec compute_recommendation() result (the ranking shown).
#' @param states one to three place names.
#' @return list(places = data.frame(State, Type, In_Ranking, Rank, Relative_Match,
#'   Limited_Coverage, Coverage_Note), criteria = long data.frame(Criterion,
#'   Label, Weight, State, Score, Position, Support) ordered by weight,
#'   indicators = long data.frame(State, Criterion, Indicator, Label, Raw,
#'   Unit_Display, Value_Text, Status, Provenance), tradeoffs = character,
#'   notes = character).
comparison_view <- function(rec, states, d, meta, low_conf = character(0), similar_within = 0.05) {
  states <- unique(states)
  if (length(states) < 1 || length(states) > 3) stop("Choose one to three States/UTs to compare.", call. = FALSE)
  bad <- setdiff(states, d$State)
  if (length(bad)) stop("Unknown State/UT: ", paste(bad, collapse = ", "), call. = FALSE)
  Xall <- criteria_matrix(d)
  res <- rec$topsis$results
  w <- rec$ahp$weights[CRITERIA]
  cs <- coverage_summary(d, meta); ct <- coverage_table(d, meta)
  inr <- states %in% rownames(rec$X)
  places <- data.frame(State = states, Type = d$Type[match(states, d$State)], In_Ranking = inr,
                       Rank = res$Rank[match(states, res$State)],
                       Relative_Match = round(res$Closeness[match(states, res$State)], 2),
                       Limited_Coverage = states %in% low_conf,
                       Coverage_Note = cs$Note[match(states, cs$State)], stringsAsFactors = FALSE)
  ord <- weights_table(rec$ahp)$Criterion
  crit <- do.call(rbind, lapply(ord, function(cn) {
    pos <- positions(rec$X, cn)
    data.frame(Criterion = cn, Label = criterion_label(cn), Weight = unname(w[cn]), State = states,
               Score = unname(Xall[states, cn]), Position = unname(pos[states]),
               Support = ct$Support[match(paste(states, cn), paste(ct$State, ct$Criterion))],
               stringsAsFactors = FALSE)
  }))
  rownames(crit) <- NULL
  ind <- do.call(rbind, lapply(states, function(st) {
    ev <- indicator_evidence(d, meta, st)
    data.frame(State = st, ev[c("Criterion", "Indicator", "Label", "Raw", "Unit_Display", "Value_Text", "Status", "Provenance")],
               stringsAsFactors = FALSE)
  }))
  rownames(ind) <- NULL
  trade <- character(0)
  if (length(states) >= 2) {
    pairs <- utils::combn(states, 2)
    for (j in seq_len(ncol(pairs))) {
      trade <- c(trade, tradeoff_sentences(Xall, pairs[1, j], pairs[2, j], w, similar_within))
    }
  }
  notes <- c(places$Coverage_Note[nzchar(places$Coverage_Note)],
             if (any(!inr)) sprintf("%s left out of your current ranking, so no rank is shown; scores are still compared.",
                                    paste(states[!inr], collapse = ", ")),
             "Scores are relative (0-1) and positions are among the places ranked; indicator values are the original data.")
  list(places = places, criteria = crit, indicators = ind, tradeoffs = trade, notes = notes)
}

#' Detail view model for one place (catalogue detail).
#' @return list(state, type, region, in_ranking, rank, n_ranked, relative_match,
#'   limited_coverage, coverage_note, criteria = data.frame(Criterion, Label,
#'   Score, Position, Position_Group, Available, Total, Support),
#'   indicators = indicator evidence data.frame, similar = similar_states()
#'   among the places ranked (unweighted, labelled as such), similar_note,
#'   standing = one plain sentence on whether another ranked place scores at
#'   least as well on every criterion).
place_detail <- function(rec, d, meta, state, low_conf = character(0), k_similar = 3) {
  if (!is.character(state) || length(state) != 1 || !state %in% d$State) {
    stop("Unknown State/UT: ", paste(state, collapse = ", "), call. = FALSE)
  }
  Xall <- criteria_matrix(d)
  inr <- state %in% rownames(rec$X)
  # Compare with the places ranked; a place left out is compared with them too.
  Xc <- if (inr) rec$X else rbind(rec$X, Xall[state, , drop = FALSE])
  n <- nrow(Xc)
  ct <- coverage_table(d, meta); ct <- ct[ct$State == state, ]
  pos <- vapply(CRITERIA, function(cn) unname(positions(Xc, cn)[state]), numeric(1))
  crit <- data.frame(Criterion = CRITERIA, Label = criterion_label(CRITERIA), Score = unname(Xall[state, ]),
                     Position = unname(pos), Position_Group = position_group(pos, n),
                     Available = ct$Available, Total = ct$Total, Support = ct$Support, stringsAsFactors = FALSE)
  fr <- pareto_front(Xc)
  doms <- attr(fr, "dominated_by")[[state]]
  standing <- if (length(doms) == 0) {
    sprintf("No other place in this comparison scores at least as well as %s on every criterion.", state)
  } else {
    sprintf("%s score%s at least as well as %s on every criterion.",
            paste(utils::head(doms, 3), collapse = ", "),
            if (length(doms) == 1) "s" else "", state)
  }
  if (length(doms) > 3) standing <- sub("score at least", sprintf("(and %d more) score at least", length(doms) - 3), standing)
  res <- rec$topsis$results
  cs <- coverage_summary(d, meta)
  list(state = state, type = d$Type[d$State == state], region = d$Region[d$State == state],
       in_ranking = inr, rank = if (inr) res$Rank[res$State == state] else NA_integer_, n_ranked = nrow(rec$X),
       relative_match = if (inr) round(res$Closeness[res$State == state], 2) else NA_real_,
       limited_coverage = state %in% low_conf, coverage_note = cs$Note[cs$State == state],
       criteria = crit, indicators = indicator_evidence(d, meta, state),
       similar = similar_states(Xc, state, k_similar),
       similar_note = "Similar overall profile across all seven scores. This does not use your priorities.",
       standing = standing)
}

#' Catalogue view model: every place with its current rank and data coverage.
#' @return catalogue_data() columns plus Limited_Coverage, Coverage_Note and Imputed.
catalogue_view <- function(rec, d, meta) {
  cat <- catalogue_data(rec, d)
  cs <- coverage_summary(d, meta)
  i <- match(cat$State, cs$State)
  cat$Limited_Coverage <- cs$Limited_Coverage[i]
  cat$Coverage_Note <- cs$Note[i]
  cat$Imputed <- cs$Imputed[i]
  cat
}

#' Stability view model: the simple single-change check, and optionally the
#' detailed robustness summary for an advanced view.
#'
#' The detail uses robustness_analysis() with EXACT enumeration of all
#' one-point answer shifts (deterministic, no seed): each answer is assumed to
#' be one point off with probability `p_change`. Shares are "shares of tested
#' answer variations under that assumption", never real-world probabilities.
#' Both parts use the places in the current ranking.
#' @return list(check = stability_check(), detail = NULL or list(summary,
#'   competitive, assumption, interpretation)).
stability_view <- function(answers, X, low_conf = character(0), exclude_low = FALSE, k = 3,
                           detail = FALSE, p_change = 0.25, flags = NULL) {
  chk <- stability_check(answers, X, low_conf, exclude_low, k)
  det <- NULL
  if (isTRUE(detail)) {
    rec <- compute_recommendation(answers, X, low_conf, exclude_low)
    rb <- robustness_analysis(rec$X, answers = answers, preference = "ratings", exact = TRUE,
                              p_change = p_change, flags = flags)
    det <- list(summary = rb$summary, competitive = rb$competitive,
                assumption = sprintf("Every combination of one-point changes to your seven answers was tested, assuming each answer has a %.0f%% chance of being one point off (half up, half down).", 100 * p_change),
                interpretation = "Shares are shares of tested answer variations under this assumption. They are not probabilities that a place is best, and they do not reflect data accuracy.")
  }
  list(check = chk, detail = det)
}

#' Share as a percentage for display: never "0%" for a share that occurred,
#' never "100%" for one that is not certain ("<1%", ">99%").
format_share <- function(x) {
  out <- sprintf("%.0f%%", 100 * x)
  ifelse(out == "0%" & x > 0, "<1%", ifelse(out == "100%" & x < 1, ">99%", out))
}

#' Plain-language view of a robustness_analysis() result (advanced view).
#'
#' Moves the wording out of the interface so any design can show it. Shares
#' are shares of simulations under the stated assumption, never probabilities.
#' @param rb result of robustness_analysis().
#' @param top_n rows to show (the baseline top 3 is always added).
#' @return list(top_state, top1_share, top3_share, n_text, headline,
#'   competitive (sentence or ""), table = data.frame of display strings
#'   (State, Your_Rank, Match, Rank_1, Top_3, Top_5, Top_10, Simulated_Rank,
#'   Data_Note), table_note, influence (sentence or ""), assumption,
#'   interpretation, data_note, not_compared).
robustness_view <- function(rb, top_n = 10) {
  s <- rb$summary
  n_txt <- format(rb$settings$n, big.mark = ",")
  base_top3 <- rb$baseline$results$State[rb$baseline$results$Rank <= 3]
  show <- rbind(utils::head(s, top_n), s[s$State %in% setdiff(base_top3, utils::head(s, top_n)$State), ])
  table <- data.frame(State = show$State, Your_Rank = show$Base_Rank, Match = sprintf("%.3f", show$Base_Closeness),
                      Rank_1 = format_share(show$Top1_Prop), Top_3 = format_share(show$Top3_Prop),
                      Top_5 = format_share(show$Top5_Prop), Top_10 = format_share(show$Top10_Prop),
                      Simulated_Rank = sprintf("%d (%d-%d)", as.integer(show$Rank_Median), as.integer(show$Rank_Q05),
                                               as.integer(show$Rank_Q95)),
                      Data_Note = show$Data_Note, row.names = NULL, stringsAsFactors = FALSE)
  infl <- rb$influence
  strong <- if (!is.null(infl)) infl[!is.na(infl$Rho_Top_Rank) & abs(infl$Rho_Top_Rank) >= 0.2, ] else NULL
  influence <- if (!is.null(strong) && nrow(strong) > 0) {
    paste0("What moves ", rb$baseline$state, " most: ",
           paste(sprintf("%s (more weight tends to move it %s)", strong$Criterion, ifelse(strong$Rho_Top_Rank > 0, "down", "up")),
                 collapse = "; "), ". This is a rank correlation across the simulations, not a cause.")
  } else ""
  st <- rb$settings
  assumption <- if (st$preference == "ratings") {
    sprintf("each of your seven ratings had a %s chance of being one point higher or lower (half each way, kept within 1-5), and the weights were recomputed with AHP",
            format_share(st$p_change))
  } else if (st$preference == "dirichlet") {
    w <- max(rb$baseline$weights)
    sprintf("your weights were drawn from a Dirichlet distribution centred on your current weights (concentration %s; a weight of %s typically moves by about %.1f percentage points)",
            st$concentration, format_share(w), 100 * dirichlet_weight_sd(w, st$concentration))
  } else "your weights were kept fixed"
  seed_txt <- if (isTRUE(st$exact)) "Every combination was enumerated, so no random numbers were used."
              else sprintf("Random seed %s gives the same numbers every time.", format(st$seed, scientific = FALSE))
  list(top_state = rb$baseline$state, top1_share = format_share(rb$baseline$top1_prop),
       top3_share = format_share(rb$baseline$top3_prop), n_text = n_txt,
       headline = if (isTRUE(st$exact)) {
         sprintf("Across all combinations of one-point changes to your answers (weighted by how likely each is), your #1, %s, stays first in %s of cases and in your top 3 in %s.",
                 rb$baseline$state, format_share(rb$baseline$top1_prop), format_share(rb$baseline$top3_prop))
       } else {
         sprintf("Your #1, %s, stays first in %s and in your top 3 in %s of %s simulations.",
                 rb$baseline$state, format_share(rb$baseline$top1_prop), format_share(rb$baseline$top3_prop), n_txt)
       },
       competitive = if (nrow(rb$competitive) > 0) paste0("Also often in your top 3: ", paste(sprintf("%s (%s)", rb$competitive$State,
                                                          format_share(rb$competitive$Top3_Prop)), collapse = ", "), ".") else "",
       table = table,
       table_note = "\"Your rank\" and \"Match score\" are your actual results. The other columns come from the re-ranked variations. Rows are sorted by top-3 share, then by your rank.",
       influence = influence,
       assumption = sprintf("Assumption: in each simulation %s. %s", assumption, seed_txt),
       interpretation = "The shares say how often each place stays near the top under these assumptions; they are not probabilities that a place is the best place to live.",
       data_note = "Data uncertainty is not modelled: the dataset records no sources, years or error ranges for its indicators (see docs/DATA_AUDIT.md). Places with missing indicators are flagged; their scores are used as stored.",
       not_compared = if (length(rb$not_comparable)) paste("Not compared (missing score):", paste(rb$not_comparable, collapse = ", "))
                      else "Every ranked State/UT has all seven criterion scores, so none was left out.")
}

#' The two criteria a trade-off view should start with: the user's two
#' highest-weighted criteria (ties keep the criterion order).
default_tradeoff_axes <- function(w) by_weight(w)[1:2]

#' Plain-language trade-offs between two criteria for the current ranking.
#'
#' Built on pareto_front() for the two criteria, but worded without
#' "Pareto"/"dominated". It ignores the other five criteria and says so.
#' @param rec compute_recommendation() result; @param x,y two different criteria.
#' @return list(x, y, frontier = data.frame(State, Rank, Score_X, Score_Y) of
#'   places no other place beats on both, ordered by the user's rank,
#'   top_state, top_on_frontier, sentences).
tradeoff_view <- function(rec, x, y) {
  if (length(x) != 1 || length(y) != 1 || !all(c(x, y) %in% colnames(rec$X))) {
    stop("Choose two criteria.", call. = FALSE)
  }
  if (x == y) stop("Choose two different criteria.", call. = FALSE)
  res <- rec$topsis$results
  front <- pareto_front(rec$X, c(x, y))
  eff <- front$State[which(front$Efficient)]
  fr <- data.frame(State = eff, Rank = res$Rank[match(eff, res$State)],
                   Score_X = unname(rec$X[eff, x]), Score_Y = unname(rec$X[eff, y]), stringsAsFactors = FALSE)
  fr <- fr[order(fr$Rank, fr$State), ]
  rownames(fr) <- NULL
  top <- top_k(rec$topsis, 1)$State
  s <- sprintf("On %s and %s together, %d of the %d places ranked cannot be beaten on both at once: %s (in your ranking order).",
               x, y, nrow(fr), nrow(rec$X), paste(fr$State, collapse = ", "))
  doms <- attr(front, "dominated_by")[[top]]
  s <- c(s, if (length(doms) == 0) {
    sprintf("Your #1, %s, is one of them: no other place scores at least as well on both.", top)
  } else {
    shown <- utils::head(doms[order(res$Rank[match(doms, res$State)])], 3)
    sprintf("Your #1, %s, is not: %s score%s at least as well on both %s and %s%s. It ranks first because of your other priorities.",
            top, paste(shown, collapse = ", "), if (length(shown) == 1) "s" else "", x, y,
            if (length(doms) > 3) sprintf(" (and %d more)", length(doms) - 3) else "")
  })
  if (nrow(fr) >= 2) {
    bx <- fr$State[which.max(fr$Score_X)]; by <- fr$State[which.max(fr$Score_Y)]
    if (bx != by) s <- c(s, sprintf("Among them, %s is strongest on %s and %s on %s: gaining on one means giving up some of the other.",
                                    bx, x, by, y))
  }
  s <- c(s, "This view uses only these two criteria; your ranking uses all seven with your weights.")
  list(x = x, y = y, frontier = fr, top_state = top, top_on_frontier = length(doms) == 0, sentences = s)
}

#' Data for a preference editor: the current answers with labels, hints and
#' the resulting weights. Changing several ratings and passing the old and new
#' answers to scenario_view() gives the before/after explanation.
#' @param answers named 1-5 answers.
#' @return data.frame(Criterion, Label, Hint, Rating, Rating_Label, Weight, Percent) in criterion order.
preferences_view <- function(answers) {
  a <- setNames(as.integer(answers[CRITERIA]), CRITERIA)
  w <- run_ahp(a)$weights[CRITERIA]
  data.frame(Criterion = CRITERIA, Label = QUESTIONS$Label, Hint = QUESTIONS$Hint, Rating = unname(a),
             Rating_Label = ANSWER_LABELS[a], Weight = unname(w), Percent = sprintf("%.0f%%", 100 * w),
             row.names = NULL, stringsAsFactors = FALSE)
}

#' Content for "About the data and limits" (one place for all of it).
#' @return list(scope, match_note, criteria = CRITERION_INFO, method,
#'   limitations, data_notes, stability_notes, before_you_move).
about_view <- function() {
  list(scope = SCOPE_NOTE, match_note = MATCH_NOTE, criteria = CRITERION_INFO, method = METHOD_STEPS,
       limitations = LIMITATIONS, data_notes = DATA_NOTES, stability_notes = STABILITY_NOTES,
       before_you_move = BEFORE_YOU_MOVE)
}

# ------------------------------------------------------------------------------
# Compact view models for the main screens (What if, Stability, Compare, Top 3)
# ------------------------------------------------------------------------------

# Display conventions (not statistical tests): two relative scores closer than
# NEAR_TIE_SCORE on the 0-1 scale are shown as "about the same"; two overall
# matches closer than NEAR_TIE_MATCH are shown as "nearly tied".
NEAR_TIE_SCORE <- 0.05
NEAR_TIE_MATCH <- 0.02
STABILITY_CAVEAT <- "This shows how sensitive your result is to your own answers. It is not the chance that a place is best for you."

#' One short data-coverage badge for a place ("" when every indicator is there),
#' e.g. "Data gaps: Connectivity estimated; Health, Housing partial".
coverage_badge <- function(d, meta, state) {
  cs <- coverage_summary(d, meta)
  r <- cs[cs$State == state, ]
  if (!nrow(r) || !nzchar(r$Note)) return("")
  parts <- c(if (nzchar(r$Imputed)) paste(r$Imputed, "estimated"),
             if (nzchar(r$Thin)) paste(r$Thin, "based on few indicators"),
             if (nzchar(r$Partial)) paste(r$Partial, "partial"))
  paste0(if (isTRUE(r$Limited_Coverage)) "Limited data: " else "Data gaps: ", paste(parts, collapse = "; "))
}

# Rank movement as a short arrow label (ASCII-safe escapes).
move_label <- function(before, after) {
  ifelse(is.na(before), "new",
         ifelse(after < before, sprintf("▲ %d", before - after),
                ifelse(after > before, sprintf("▼ %d", after - before), "same")))
}

#' What-if view model: the user's original answers (baseline) against a set of
#' alternative answers, ranked by the same engine with the same exclusion rule.
#'
#' @param baseline named 1-5 original answers.
#' @param answers named 1-5 alternative answers (any number of them changed).
#' @param X,low_conf,exclude_low as in compute_recommendation().
#' @return list(unchanged, changed = data.frame(Criterion, From, To, Weight_Before,
#'   Weight_After), before = data.frame(Rank, State, Status) for the original
#'   top k ("stays" / "leaves"), after = data.frame(Rank, State, Was, Move,
#'   New) for the new top k, moves (all places in either list: State, Before,
#'   After, Move), entered, left, new_top, summary (one sentence),
#'   explanation (character, grounded in the weights and the scores)).
whatif_view <- function(baseline, answers, X, low_conf = character(0), exclude_low = FALSE, k = 3) {
  a <- setNames(suppressWarnings(as.integer(answers[CRITERIA])), CRITERIA)
  if (length(answers) != length(CRITERIA) || anyNA(a) || any(a < 1 | a > 5)) {
    stop("Each rating must be a whole number from 1 to 5 for all seven criteria.", call. = FALSE)
  }
  sv <- scenario_view(baseline, a, X, low_conf, exclude_low, k)
  sc <- sv$scenario
  rb <- sc$base$topsis$results; ra <- sc$scenario$topsis$results
  tb <- sv$top_before; ta <- sv$top_after
  before <- data.frame(Rank = rb$Rank[match(tb, rb$State)], State = tb,
                       Status = ifelse(tb %in% ta, "stays", "leaves"), stringsAsFactors = FALSE)
  was <- rb$Rank[match(ta, rb$State)]
  after <- data.frame(Rank = ra$Rank[match(ta, ra$State)], State = ta, Was = was,
                      Move = move_label(was, ra$Rank[match(ta, ra$State)]), New = !ta %in% tb,
                      stringsAsFactors = FALSE)
  moves <- sv$moves
  moves$Move <- move_label(moves$Before, moves$After)
  if (nrow(sv$changed) == 0) {
    return(list(unchanged = TRUE, changed = sv$changed, before = before, after = after, moves = moves,
                entered = character(0), left = character(0), new_top = FALSE,
                summary = "These are your original answers. Change at least one rating to test a different set of priorities.",
                explanation = character(0)))
  }
  wb <- sc$base$ahp$weights[CRITERIA]; wa <- sc$scenario$ahp$weights[CRITERIA]
  S <- sc$scenario$X
  expl <- sprintf("%s now counts %.0f%% of your result (was %.0f%%).", sv$changed$Criterion,
                  100 * sv$changed$Weight_After, 100 * sv$changed$Weight_Before)
  # Why a place moved ahead: the criterion where the weight shift helped it most
  # against the place it overtook (weight change x score difference).
  driver <- function(winner, loser) {
    if (!all(c(winner, loser) %in% rownames(S))) return(character(0))
    gain <- (wa - wb) * (S[winner, ] - S[loser, ])
    cn <- names(which.max(gain))
    if (gain[cn] <= 0) return(character(0))
    sprintf("%s moves ahead of %s mainly because %s counts %s now, and it scores %.2f there against %.2f.",
            winner, loser, cn, if (wa[cn] > wb[cn]) "more" else "less", S[winner, cn], S[loser, cn])
  }
  if (sv$new_top) {
    expl <- c(expl, driver(ta[1], tb[1]))
  } else if (length(sv$entered)) {
    expl <- c(expl, driver(sv$entered[1], sv$left[1]))
  }
  summary <- if (sv$new_top) sprintf("With these priorities your #1 changes from %s to %s.", tb[1], ta[1])
             else if (length(sv$entered)) sprintf("Your #1 stays %s; %s enter%s your top %d.", tb[1],
                                                 paste(sv$entered, collapse = " and "), if (length(sv$entered) == 1) "s" else "", k)
             else if (!identical(ta, tb)) sprintf("Same top %d, in a different order.", k)
             else sprintf("Your top %d does not change.", k)
  list(unchanged = FALSE, changed = sv$changed, before = before, after = after, moves = moves,
       entered = sv$entered, left = sv$left, new_top = sv$new_top, summary = summary, explanation = expl)
}

#' Stability summary for the main screen: one label, one headline, a bar per
#' place and one sentence, built on the existing engines (stability_check()
#' and robustness_analysis(exact = TRUE): every combination of one-point
#' answer changes, weighted by `p_change`; deterministic, no seed).
#'
#' Labels (display conventions on the #1's share of variations):
#'   "Holds up" >= 0.9, "Usually holds" >= 0.5, "Close call" < 0.5.
#' @return list(top_state, label, label_class ("strong"/"moderate"/"short"),
#'   top1_share, headline, sentence, places = data.frame(State, Rank,
#'   First_Share, Top3_Share, First_Label, Top3_Label, Badge), caveat, explain,
#'   check = stability_check() result).
stability_summary_view <- function(answers, X, d, meta, low_conf = character(0), exclude_low = FALSE,
                                   p_change = 0.25, max_places = 5) {
  chk <- stability_check(answers, X, low_conf, exclude_low, 3)
  rec <- compute_recommendation(answers, X, low_conf, exclude_low)
  rb <- robustness_analysis(rec$X, answers = answers, preference = "ratings", exact = TRUE, p_change = p_change)
  rv <- robustness_view(rb)
  s <- rb$summary
  top <- rb$baseline$state
  share <- rb$baseline$top1_prop
  label <- if (share >= 0.9) "Holds up" else if (share >= 0.5) "Usually holds" else "Close call"
  cls <- c("Holds up" = "strong", "Usually holds" = "moderate", "Close call" = "short")[[label]]
  show <- unique(c(chk$shortlist$State, chk$also_consider$State, rb$competitive$State))
  show <- utils::head(show[order(s$Base_Rank[match(show, s$State)], show)], max_places)   # your rank order
  places <- data.frame(State = show, Rank = s$Base_Rank[match(show, s$State)],
                       First_Share = s$Top1_Prop[match(show, s$State)], Top3_Share = s$Top3_Prop[match(show, s$State)],
                       stringsAsFactors = FALSE)
  places$First_Label <- format_share(places$First_Share)
  places$Top3_Label <- format_share(places$Top3_Share)
  places$Badge <- vapply(places$State, function(st) coverage_badge(d, meta, st), "", USE.NAMES = FALSE)
  sentence <- if (chk$top1_kept == chk$n_variants && !chk$close_call) {
    "Small changes to your answers do not change your top 3."
  } else if (chk$top1_kept == chk$n_variants) {
    sprintf("Small changes to your answers keep %s first, but can reorder the rest of your top 3.", top)
  } else {
    "Small changes to your answers can change your #1, so compare the places below before deciding."
  }
  list(top_state = top, label = label, label_class = cls, top1_share = share,
       headline = sprintf("%s stays your #1 in %s of the answer variations we tested.", top, rv$top1_share),
       sentence = sentence, places = places, caveat = STABILITY_CAVEAT,
       explain = c("We re-ranked your results with every combination of one-point changes to your seven answers, using the same method and the same places.",
                   sprintf("\"How sure are you?\" sets how likely each change is (%s chance per answer).", format_share(p_change)),
                   "The bars show how often each place came first, and how often it stayed in your top 3, across those variations."),
       check = chk)
}

#' Comparison bars for two or three places: one row per criterion (ordered by
#' the user's weight) with each place's relative score, the clear leader (if
#' any) and coverage flags, plus up to two plain observations.
#' @return list(places = comparison_view()$places, bars = data.frame(Criterion,
#'   Weight, Percent, State, Score, Position, Flag, Leads, Tie), observations,
#'   badges = named character (coverage_badge per place), indicators =
#'   comparison_view()$indicators (for an optional detail view), n_ranked).
comparison_bars_view <- function(rec, states, d, meta, low_conf = character(0)) {
  cv <- comparison_view(rec, states, d, meta, low_conf)
  cr <- cv$criteria
  flag <- c(full = "", partial = "some data missing", thin = "few indicators", imputed = "estimated, not measured")
  cr$Flag <- unname(flag[cr$Support])
  cr$Percent <- sprintf("%.0f%%", 100 * cr$Weight)
  rng <- tapply(cr$Score, cr$Criterion, function(x) max(x) - min(x))
  cr$Tie <- unname(rng[cr$Criterion] < NEAR_TIE_SCORE)
  cr$Leads <- vapply(seq_len(nrow(cr)), function(i) {
    x <- cr$Score[cr$Criterion == cr$Criterion[i]]
    !cr$Tie[i] && cr$Score[i] == max(x) && sum(x == max(x)) == 1 && max(x) - max(x[x < max(x)], -Inf) >= NEAR_TIE_SCORE
  }, logical(1))
  bars <- cr[c("Criterion", "Weight", "Percent", "State", "Score", "Position", "Flag", "Leads", "Tie")]
  obs <- character(0)
  if (length(unique(states)) >= 2) {
    w <- tapply(cr$Weight, cr$Criterion, `[`, 1)
    imp <- names(which.max(w * rng[names(w)]))
    if (rng[imp] < NEAR_TIE_SCORE) {
      obs <- c(obs, "On the criteria that matter most to you, these places are about the same.")
    } else {
      lead <- cr$State[cr$Criterion == imp][which.max(cr$Score[cr$Criterion == imp])]
      obs <- c(obs, sprintf("The biggest difference for your priorities is %s (%s of your result): %s is clearly ahead.",
                            imp, sprintf("%.0f%%", 100 * w[[imp]]), lead))
    }
    pl <- cv$places[cv$places$In_Ranking, ]
    pl <- pl[order(pl$Rank), ]
    res <- rec$topsis$results
    if (nrow(pl) >= 2) {
      gap <- abs(diff(res$Closeness[match(pl$State[1:2], res$State)]))
      obs <- c(obs, if (gap < NEAR_TIE_MATCH) sprintf("%s and %s are nearly tied overall for your priorities.", pl$State[1], pl$State[2])
                    else sprintf("Overall, %s matches your priorities best of these (rank %d of %d).", pl$State[1], pl$Rank[1], nrow(rec$X)))
    }
  }
  list(places = cv$places, bars = bars, observations = obs,
       badges = setNames(vapply(cv$places$State, function(st) coverage_badge(d, meta, st), ""), cv$places$State),
       indicators = cv$indicators, n_ranked = nrow(rec$X))
}

#' Chart data for the two-criterion trade-off explorer (Compare).
#'
#' One point per place in the current ranking (`rec$X`, so the exclusion rule
#' applies), placed at its relative scores on two criteria. Built on
#' tradeoff_view() (which uses pareto_front()) and the data-quality functions;
#' no new scoring. Places with a missing score on either axis are listed in
#' `not_plotted` and never drawn at zero.
#'
#' @param rec compute_recommendation() result.
#' @param x,y two different criteria (horizontal, vertical).
#' @param d,meta dataset and metadata (coverage warnings).
#' @param selected places the user selected (any order; at most three are used).
#' @param low_conf limited-coverage places.
#' @return list(x, y, x_title, y_title, n_ranked, points = data.frame(State,
#'   Short, X, Y, Rank, Match, Top3, Selected, Frontier, Support_X, Support_Y,
#'   Estimated, Partial, Limited), cards = data.frame(State, Rank, Match,
#'   Score_X, Score_Y, Pos_X, Pos_Y, Observation, Warning) in selection order,
#'   selected, excluded_selected, not_plotted, note).
tradeoff_chart_data <- function(rec, x, y, d, meta, selected = character(0), low_conf = character(0)) {
  if (length(x) != 1 || length(y) != 1 || !all(c(x, y) %in% CRITERIA)) stop("Choose two criteria.", call. = FALSE)
  if (identical(x, y)) stop("Choose two different criteria.", call. = FALSE)
  Xr <- rec$X
  res <- rec$topsis$results
  n <- nrow(Xr)
  ok <- stats::complete.cases(Xr[, c(x, y), drop = FALSE])
  tv <- tradeoff_view(list(X = Xr[ok, , drop = FALSE],
                           topsis = list(results = res[res$State %in% rownames(Xr)[ok], ])), x, y)
  ct <- coverage_table(d, meta)
  sup <- function(st, cn) ct$Support[match(paste(st, cn), paste(ct$State, ct$Criterion))]
  selected <- unique(as.character(selected))
  selected <- selected[selected %in% d$State]
  excluded_selected <- setdiff(selected, rownames(Xr))
  st <- rownames(Xr)[ok]
  # Cards only for places that can be drawn (a missing axis score is listed in
  # not_plotted instead of showing "NA").
  selected <- utils::head(intersect(selected, st), 3)
  pts <- data.frame(State = st, Short = unname(short_name(st)), X = unname(Xr[st, x]), Y = unname(Xr[st, y]),
                    Rank = res$Rank[match(st, res$State)], Match = round(res$Closeness[match(st, res$State)], 2),
                    stringsAsFactors = FALSE)
  pts$Top3 <- pts$Rank <= 3
  pts$Selected <- pts$State %in% selected
  pts$Frontier <- pts$State %in% tv$frontier$State
  pts$Support_X <- sup(st, x); pts$Support_Y <- sup(st, y)
  pts$Estimated <- pts$Support_X == "imputed" | pts$Support_Y == "imputed"
  pts$Partial <- pts$Support_X %in% c("partial", "thin") | pts$Support_Y %in% c("partial", "thin")
  pts$Limited <- pts$State %in% low_conf
  posx <- rank(-Xr[, x], ties.method = "min"); posy <- rank(-Xr[, y], ties.method = "min")
  observe1 <- function(s) {
    px <- unname(posx[s]); py <- unname(posy[s])
    gx <- position_group(px, n); gy <- position_group(py, n)
    first <- if (gx == "Top third" && gy == "Top third") sprintf("In the top third on both %s and %s.", x, y)
    else if (setequal(c(gx, gy), c("Top third", "Bottom third"))) {
      strong <- if (gx == "Top third") x else y; weak <- if (gx == "Top third") y else x
      sprintf("A clear trade-off: strong on %s (%s) but weak on %s (%s).", strong, ordinal(if (strong == x) px else py),
              weak, ordinal(if (weak == x) px else py))
    } else if (px < py) sprintf("Stronger on %s (%s) than on %s (%s).", x, ordinal(px), y, ordinal(py))
    else if (py < px) sprintf("Stronger on %s (%s) than on %s (%s).", y, ordinal(py), x, ordinal(px))
    else sprintf("Same position on %s and %s (%s).", x, y, ordinal(px))
    if (s %in% tv$frontier$State) paste(first, sprintf("No other place scores at least as well on both."))
    else first
  }
  warn1 <- function(s) {
    w <- c(vapply(c(x, y), function(cn) switch(sup(s, cn), imputed = sprintf("%s score is estimated, not measured.", cn),
                                                 thin = sprintf("%s is based on few indicators.", cn),
                                                 partial = sprintf("%s is based on only some of its indicators.", cn), ""), ""))
    w <- w[nzchar(w)]
    if (length(w)) paste(w, collapse = " ") else if (s %in% low_conf) coverage_badge(d, meta, s) else ""
  }
  cards <- data.frame(State = selected, Rank = res$Rank[match(selected, res$State)],
                      Match = round(res$Closeness[match(selected, res$State)], 2),
                      Score_X = unname(Xr[selected, x]), Score_Y = unname(Xr[selected, y]),
                      Pos_X = unname(posx[selected]), Pos_Y = unname(posy[selected]),
                      Observation = vapply(selected, observe1, "", USE.NAMES = FALSE),
                      Warning = vapply(selected, warn1, "", USE.NAMES = FALSE), stringsAsFactors = FALSE)
  list(x = x, y = y, x_title = sprintf("%s (relative score)", x), y_title = sprintf("%s (relative score)", y),
       n_ranked = n, points = pts, cards = cards, selected = selected, excluded_selected = excluded_selected,
       not_plotted = rownames(Xr)[!ok],
       note = sprintf("Scores run from 0 to 1 among the %d places ranked (higher is better on both axes); they are relative model scores, not original measurements.", n))
}

#' Toggle a clicked place in the comparison selection (at most `max_n`; adding
#' one more drops the earliest). Unknown names leave the selection unchanged.
toggle_place <- function(current, clicked, valid, max_n = 3) {
  current <- intersect(as.character(current), valid)
  if (length(clicked) != 1 || is.na(clicked) || !clicked %in% valid) return(current)
  if (clicked %in% current) setdiff(current, clicked) else utils::tail(c(current, clicked), max_n)
}

#' Keep the two axes different: when the user picks the other axis's
#' criterion, the axes swap (using the previous pair); returns the pair to use.
resolve_axes <- function(x, y, previous = NULL) {
  if (!identical(x, y)) return(c(x, y))
  if (!is.null(previous) && !identical(previous[1], x)) return(c(x, previous[1]))   # x changed: swap
  if (!is.null(previous)) return(c(previous[2], y))                                 # y changed: swap
  c(x, setdiff(CRITERIA, x)[1])
}
