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
  reasons <- setNames(lapply(top$State, function(st) {
    pb <- priority_breakdown(rec$topsis, w, st, 3)
    ev <- indicator_evidence(d, meta, st)
    pb$Label <- criterion_label(pb$Criterion)
    pb$Support <- ct$Support[match(paste(st, pb$Criterion), paste(ct$State, ct$Criterion))]
    pb$Evidence <- vapply(pb$Criterion, function(cn) paste(ev$Evidence[ev$Criterion == cn], collapse = "; "), "")
    pb[c("Criterion", "Label", "Weight", "Score", "Position", "Verdict", "Support", "Sentence", "Evidence")]
  }), top$State)
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
       headline = sprintf("Your #1, %s, stays first in %s and in your top 3 in %s of %s simulations.",
                          rb$baseline$state, format_share(rb$baseline$top1_prop), format_share(rb$baseline$top3_prop), n_txt),
       competitive = if (nrow(rb$competitive) > 0) paste0("Also often in your top 3: ", paste(sprintf("%s (%s)", rb$competitive$State,
                                                          format_share(rb$competitive$Top3_Prop)), collapse = ", "), ".") else "",
       table = table,
       table_note = "\"Your rank\" and \"Match score\" are your actual results. The other columns are simulated. Rows are sorted by top-3 share, then by your rank.",
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
