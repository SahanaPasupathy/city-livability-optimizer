# View-model contracts (R/view_models.R), the stability check and the
# multi-change scenario view (R/scenario.R), and the consistency of the
# included/excluded set across every feature.

FAM <- setNames(as.integer(PERSONAS[["Family"]]), CRITERIA)

test_that("the stability check counts single-answer changes and is deterministic", {
  s <- stability_check(EQUAL_ANSWERS, XM, LOWC)
  expect_equal(s$n_variants, 14)                                        # 7 criteria x up/down
  expect_equal(stability_check(setNames(rep(5L, 7), CRITERIA), XM, LOWC)$n_variants, 7)
  expect_identical(s, stability_check(EQUAL_ANSWERS, XM, LOWC))
  # Recount by hand with compute_recommendation().
  tops <- list()
  for (cn in CRITERIA) for (st in c(-1L, 1L)) {
    a <- EQUAL_ANSWERS; a[cn] <- a[cn] + st
    tops[[length(tops) + 1]] <- top_k(compute_recommendation(a, XM, LOWC)$topsis, 3)$State
  }
  base <- top_k(compute_recommendation(EQUAL_ANSWERS, XM, LOWC)$topsis, 3)$State
  expect_equal(s$shortlist$State, base)
  expect_equal(unname(s$shortlist$Stayed_Top_k),
               unname(vapply(base, function(b) sum(vapply(tops, function(t) b %in% t, TRUE)), 1L)))
  expect_equal(s$top1_kept, sum(vapply(tops, function(t) t[1] == base[1], TRUE)))
  expect_equal(s$close_call, any(vapply(tops, function(t) !identical(t, base), TRUE)))
  expect_false(any(s$also_consider$State %in% base))
  expect_false(any(grepl("probab|chance|confiden", s$sentences, ignore.case = TRUE)))
  expect_error(stability_check(EQUAL_ANSWERS[1:6], XM, LOWC), "seven answers")
})

test_that("the stability check respects the exclusion setting", {
  s <- stability_check(setNames(as.integer(PERSONAS[["Retiree"]]), CRITERIA), XM, LOWC, exclude_low = TRUE)
  every_top <- unlist(strsplit(s$variants$Top_k, ", "))
  expect_false(any(every_top %in% LOWC))
  expect_false(any(s$also_consider$State %in% LOWC))
})

test_that("scenario_view handles several changed ratings with the same engine", {
  sv <- scenario_view(FAM, c(Safety = 2L, Environment = 5L), XM, LOWC)
  expect_equal(sv$changed$Criterion, c("Safety", "Environment"))
  expect_equal(sv$changed$From, c(5L, 3L)); expect_equal(sv$changed$To, c(2L, 5L))
  new_answers <- FAM; new_answers[c("Safety", "Environment")] <- c(2L, 5L)
  fresh <- top_k(compute_recommendation(new_answers, XM, LOWC)$topsis, 3)$State
  expect_equal(sv$top_after, fresh)
  expect_equal(sv$top_before, c("Himachal Pradesh", "Chandigarh", "Punjab"))
  expect_setequal(sv$entered, setdiff(fresh, sv$top_before))
  expect_equal(sv$moves$Change, sv$moves$Before - sv$moves$After)
  # A complete set of new answers works too, and identical answers report no change.
  expect_equal(scenario_view(FAM, new_answers, XM, LOWC)$top_after, fresh)
  same <- scenario_view(FAM, FAM, XM, LOWC)
  expect_equal(nrow(same$changed), 0)
  expect_match(same$sentences, "nothing changed")
  expect_error(scenario_view(FAM, c(Safety = 7L), XM, LOWC), "whole number from 1 to 5")
})

test_that("recommendation_view agrees with the engine and explains with real evidence", {
  v <- recommendation_view(FAM, XM, DATA, META, LOWC)
  rec <- compute_recommendation(FAM, XM, LOWC)
  expect_equal(v$shortlist$State, top_k(rec$topsis, 3)$State)
  expect_equal(v$shortlist$Relative_Match, round(top_k(rec$topsis, 3)$Closeness, 2))
  expect_equal(v$shortlist$Limited_Coverage, v$shortlist$State %in% LOWC)
  r1 <- v$reasons[["Himachal Pradesh"]]
  expect_equal(r1$Verdict, position_group(r1$Position, 36))
  expect_equal(r1$Score, unname(XM["Himachal Pradesh", r1$Criterion]))
  expect_match(r1$Evidence[r1$Criterion == "Education"], "Grade 8 learning levels: 79.5 percent")
  expect_match(r1$Evidence[r1$Criterion == "Education"], "source not traced")    # Education index
  expect_equal(sum(v$weights$Weight), 1)
  expect_equal(v$weights$Percent[1], sprintf("%.0f%%", 100 * max(rec$ahp$weights)))
  # Chandigarh (limited coverage) is in the top 3, so the alternative is offered.
  expect_equal(v$alternative_without_limited, top_k(compute_recommendation(FAM, XM, LOWC, TRUE)$topsis, 3)$State)
  expect_null(recommendation_view(FAM, XM, DATA, META, LOWC, exclude_low = TRUE)$alternative_without_limited)
  expect_true(all(c(MATCH_NOTE, SCOPE_NOTE) %in% v$notes))
  expect_identical(v$shortlist, recommendation_view(FAM, XM, DATA, META, LOWC)$shortlist)
})

test_that("priority verdicts are positions, not score thresholds", {
  expect_equal(position_group(c(1, 12, 13, 24, 25, 36), 36),
               c("Top third", "Top third", "Middle third", "Middle third", "Bottom third", "Bottom third"))
  safety_first <- compute_recommendation(setNames(c(1L, 5L, 1L, 1L, 1L, 1L, 1L), CRITERIA), XM, LOWC)
  pb <- priority_breakdown(safety_first$topsis, safety_first$ahp$weights, "Delhi", 1)
  expect_equal(pb$Verdict, "Bottom third")
  expect_false(any(grepl("Strong match|Falls short", pb$Sentence)))
})

test_that("comparison_view gives plain trade-offs and validates its input", {
  rec <- compute_recommendation(FAM, XM, LOWC)
  cv <- comparison_view(rec, c("Himachal Pradesh", "Kerala"), DATA, META, LOWC)
  expect_equal(nrow(cv$criteria), 14)
  expect_equal(unique(cv$criteria$Criterion), weights_table(rec$ahp)$Criterion)
  expect_match(cv$tradeoffs[1], "no overall winner")
  expect_false(any(grepl("Pareto|dominat", cv$tradeoffs)))
  expect_equal(nrow(cv$indicators), 34)
  expect_equal(cv$indicators$Raw[cv$indicators$State == "Kerala" & cv$indicators$Indicator == "MPI_Headcount_Pct"], 0.55)
  # A one-sided pair, found with pareto_front() on the real data.
  fr <- pareto_front(XM)
  dom <- attr(fr, "dominated_by")
  loser <- names(dom)[lengths(dom) > 0][1]; winner <- dom[[loser]][1]
  one <- comparison_view(rec, c(winner, loser), DATA, META, LOWC)$tradeoffs[1]
  expect_equal(one, sprintf("On every criterion, %s scores at least as well as %s.", winner, loser))
  expect_error(comparison_view(rec, character(0), DATA, META), "one to three")
  expect_error(comparison_view(rec, c("Goa", "Kerala", "Punjab", "Bihar"), DATA, META), "one to three")
  expect_error(comparison_view(rec, "Atlantis", DATA, META), "Unknown")
})

test_that("every feature uses the same included/excluded set (regression)", {
  rec <- compute_recommendation(setNames(as.integer(PERSONAS[["Retiree"]]), CRITERIA), XM, LOWC, exclude_low = TRUE)
  expect_equal(nrow(rec$X), 32)
  # Comparison: a place left out stays comparable but has no rank or position.
  cv <- comparison_view(rec, c("Karnataka", "Lakshadweep"), DATA, META, LOWC)
  expect_equal(cv$places$In_Ranking, c(TRUE, FALSE))
  expect_true(is.na(cv$places$Rank[2]))
  expect_true(all(is.na(cv$criteria$Position[cv$criteria$State == "Lakshadweep"])))
  expect_true(all(cv$criteria$Position[cv$criteria$State == "Karnataka"] <= 32))
  # Details: positions among the 32 ranked (plus the place itself when left out);
  # similar places never include places left out.
  pd <- place_detail(rec, DATA, META, "Karnataka", LOWC)
  expect_equal(pd$n_ranked, 32)
  expect_false(any(pd$similar$State %in% LOWC))
  expect_equal(pd$criteria$Position, unname(vapply(CRITERIA, function(cn) rank(-rec$X[, cn], ties.method = "min")["Karnataka"], 1)))
  pl <- place_detail(rec, DATA, META, "Lakshadweep", LOWC)
  expect_false(pl$in_ranking); expect_true(is.na(pl$rank))
  expect_true(all(pl$criteria$Position <= 33))
  expect_false(any(pl$similar$State %in% setdiff(LOWC, "Lakshadweep")))
  # Catalogue, stability and robustness detail.
  cat <- catalogue_view(rec, DATA, META)
  expect_true(all(is.na(cat$Rank[cat$State %in% LOWC])))
  expect_equal(cat$Imputed[cat$State == "Lakshadweep"], "Connectivity")
  sv <- stability_view(setNames(as.integer(PERSONAS[["Retiree"]]), CRITERIA), XM, LOWC, TRUE, detail = TRUE)
  expect_equal(nrow(sv$detail$summary), 32)
  expect_false(any(sv$detail$summary$State %in% LOWC))
  expect_true(all(sv$detail$summary$Top3_Prop >= 0 & sv$detail$summary$Top3_Prop <= 1))
  expect_match(sv$detail$interpretation, "not probabilities")
})

test_that("place_detail's standing sentence agrees with pareto_front()", {
  rec <- compute_recommendation(FAM, XM, LOWC)
  fr <- pareto_front(rec$X)
  eff <- fr$State[fr$Efficient][1]; dominated <- fr$State[!fr$Efficient][1]
  expect_match(place_detail(rec, DATA, META, eff, LOWC)$standing, "^No other place")
  doms <- attr(fr, "dominated_by")[[dominated]]
  expect_match(place_detail(rec, DATA, META, dominated, LOWC)$standing, doms[1], fixed = TRUE)
  expect_error(place_detail(rec, DATA, META, "Atlantis"), "Unknown")
})

test_that("the map colour scale is fixed at 0-1 and monotonic", {
  a <- closeness_colours(c(0.40, 0.42, 0.44))
  b <- closeness_colours(c(0.40, 0.90))
  expect_equal(a[1], b[1])                          # same value, same colour, whatever the others
  ends <- grDevices::col2rgb(closeness_colours(c(0, 1)))             # Lab round-trip: within 2 per channel
  expect_lte(max(abs(ends - grDevices::col2rgb(c(MAP_RAMP[1], MAP_RAMP[length(MAP_RAMP)])))), 2)
  br <- colSums(grDevices::col2rgb(closeness_colours(seq(0, 1, 0.1))))
  expect_false(is.unsorted(rev(br)))
  # The old stretched scale is still available on request.
  expect_lte(max(abs(grDevices::col2rgb(closeness_colours(c(0.40, 0.44), limits = NULL)[2]) -
                       grDevices::col2rgb(MAP_RAMP[length(MAP_RAMP)]))), 2)
})

test_that("the weights message explains magnification instead of quoting the consistency ratio", {
  fam <- run_ahp(FAM)
  msg <- consistency_message(fam)
  expect_false(grepl("ratio", msg, ignore.case = TRUE))
  expect_match(msg, "times as much")
  expect_match(consistency_message(run_ahp(EQUAL_ANSWERS)), "counts equally")
})

test_that("robustness_view words the engine's results without changing them", {
  rec <- compute_recommendation(FAM, XM, LOWC)
  rb <- robustness_analysis(rec$X, answers = FAM, preference = "ratings", n = 500, seed = 7, p_change = 0.25)
  v <- robustness_view(rb)
  expect_equal(v$top_state, rb$baseline$state)
  expect_equal(v$top3_share, format_share(rb$baseline$top3_prop))
  expect_match(v$headline, sprintf("stays first in %s and in your top 3 in %s of 500 simulations", v$top1_share, v$top3_share), fixed = TRUE)
  expect_true(all(top_k(rec$topsis, 3)$State %in% v$table$State))
  expect_equal(v$table$Top_3[1], format_share(rb$summary$Top3_Prop[1]))
  expect_match(v$assumption, "Random seed 7")
  expect_match(v$interpretation, "not probabilities")
  ex <- robustness_view(robustness_analysis(rec$X, answers = FAM, preference = "ratings", exact = TRUE, p_change = 0.25))
  expect_match(ex$assumption, "no random numbers")
  expect_match(ex$headline, "Across all combinations of one-point changes")
  expect_false(grepl("simulations", ex$headline))
  expect_equal(format_share(c(0, 0.004, 0.5, 0.996, 1)), c("0%", "<1%", "50%", ">99%", "100%"))
})

test_that("tradeoff_view explains two-criterion trade-offs in plain words", {
  rec <- compute_recommendation(setNames(c(4L, 3L, 5L, 2L, 4L, 1L, 3L), CRITERIA), XM, LOWC)
  tv <- tradeoff_view(rec, "Safety", "Economy")
  front <- pareto_front(rec$X, c("Safety", "Economy"))
  expect_setequal(tv$frontier$State, front$State[front$Efficient])
  expect_false(is.unsorted(tv$frontier$Rank))
  expect_equal(tv$top_on_frontier, isTRUE(front$Efficient[front$State == tv$top_state]))
  expect_false(any(grepl("Pareto|dominat", tv$sentences)))
  expect_match(tv$sentences[length(tv$sentences)], "uses only these two criteria")
  expect_error(tradeoff_view(rec, "Safety", "Safety"), "different")
  expect_error(tradeoff_view(rec, "Safety", "Wealth"), "two criteria")
  # With places left out, the frontier only contains ranked places.
  ex <- compute_recommendation(setNames(as.integer(PERSONAS[["Retiree"]]), CRITERIA), XM, LOWC, TRUE)
  expect_false(any(tradeoff_view(ex, "Health", "Environment")$frontier$State %in% LOWC))
  expect_equal(default_tradeoff_axes(run_ahp(FAM)$weights), c("Education", "Safety"))
})

test_that("preferences_view and about_view expose the editor and limits content", {
  pv <- preferences_view(FAM)
  expect_equal(pv$Criterion, CRITERIA)
  expect_equal(pv$Rating, unname(FAM))
  expect_equal(sum(pv$Weight), 1)
  expect_equal(pv$Rating_Label[pv$Criterion == "Connectivity"], "Slightly")
  av <- about_view()
  expect_equal(av$limitations, LIMITATIONS)
  expect_equal(av$before_you_move, BEFORE_YOU_MOVE)
  expect_equal(av$criteria$Criterion, CRITERIA)
})

test_that("the report includes what changed compared with the original answers", {
  new <- FAM; new[c("Safety", "Environment")] <- c(2L, 5L)
  rec <- compute_recommendation(new, XM, LOWC)
  chg <- scenario_view(FAM, new, XM, LOWC)
  html <- build_report_html(rec, XM, LOWC, "edited", NULL, d = DATA, meta = META, changes = chg)
  expect_true(grepl("What changed compared with my original answers", html, fixed = TRUE))
  expect_true(grepl("Safety from 5 to 2", html, fixed = TRUE))
  expect_equal(lengths(regmatches(html, gregexpr("<tr>\\s*<td>", html)))[1], 36)   # ranking rows untouched
  expect_false(grepl("What changed", build_report_html(rec, XM, LOWC, "edited", NULL, changes = scenario_view(new, new, XM, LOWC))))
})

test_that("map hover cards carry data notes, including for places left out", {
  md <- load_map_paths(file.path(ROOT_DIR, "data", "map", "india_map_paths.csv"))
  notes <- setNames(coverage_summary(DATA, META)$Note, DATA$State); notes <- notes[nzchar(notes)]
  rec <- compute_recommendation(FAM, XM, LOWC)
  html <- as.character(render_india_map(rec$topsis$results, XM, LOWC, md, notes = notes))
  expect_true(grepl("no measured data for Connectivity", html, fixed = TRUE))
  ex <- compute_recommendation(FAM, XM, LOWC, TRUE)
  html_ex <- as.character(render_india_map(ex$topsis$results, XM, LOWC, md, notes = notes))
  expect_true(grepl("no measured data for Connectivity", html_ex, fixed = TRUE))       # Lakshadweep left out
  expect_equal(lengths(regmatches(html_ex, gregexpr("Excluded from this ranking", html_ex, fixed = TRUE))), 7)
  plain <- as.character(render_india_map(rec$topsis$results, XM, LOWC, md))
  expect_false(grepl("no measured data", plain, fixed = TRUE))
})

ORIG <- setNames(c(4L, 3L, 5L, 2L, 4L, 1L, 3L), CRITERIA)

test_that("whatif_view compares several changed ratings with the original answers", {
  new <- ORIG; new[c("Safety", "Environment")] <- c(5L, 4L)
  orig_copy <- ORIG
  wv <- whatif_view(ORIG, new, XM, LOWC)
  expect_identical(ORIG, orig_copy)                                   # baseline untouched
  expect_false(wv$unchanged)
  expect_equal(wv$changed$Criterion, c("Safety", "Environment"))
  base <- top_k(compute_recommendation(ORIG, XM, LOWC)$topsis, 3)$State
  scen <- top_k(compute_recommendation(new, XM, LOWC)$topsis, 3)$State
  expect_equal(wv$before$State, base)
  expect_equal(wv$after$State, scen)                                  # same engine
  expect_equal(wv$before$Status, ifelse(base %in% scen, "stays", "leaves"))
  expect_equal(wv$after$New, !scen %in% base)
  res0 <- compute_recommendation(ORIG, XM, LOWC)$topsis$results
  was <- res0$Rank[match(scen, res0$State)]
  expect_equal(wv$after$Was, was)
  expect_equal(wv$after$Move, move_label(was, wv$after$Rank))
  expect_equal(move_label(c(5, 1, 3, NA), c(1, 4, 3, 2)), c("▲ 4", "▼ 3", "same", "new"))
  expect_true(wv$new_top)
  expect_match(wv$summary, "changes from Delhi to Lakshadweep")
  expect_match(wv$explanation[1], "Safety now counts 29% of your result \\(was 8%\\)")
  expect_true(any(grepl("Lakshadweep moves ahead of Delhi mainly because Safety counts more", wv$explanation)))
})

test_that("whatif_view handles unchanged answers, invalid ratings and the exclusion rule", {
  same <- whatif_view(ORIG, ORIG, XM, LOWC)
  expect_true(same$unchanged)
  expect_length(same$explanation, 0)
  expect_match(same$summary, "original answers")
  bad <- ORIG; bad["Safety"] <- 7L
  expect_error(whatif_view(ORIG, bad, XM, LOWC), "whole number from 1 to 5")
  expect_error(whatif_view(ORIG, ORIG[1:6], XM, LOWC), "all seven")
  new <- ORIG; new["Safety"] <- 5L
  ex <- whatif_view(ORIG, new, XM, LOWC, exclude_low = TRUE)
  expect_false(any(c(ex$before$State, ex$after$State, ex$moves$State) %in% LOWC))
  # Ties: equal answers everywhere still give a valid, deterministic comparison.
  eq <- whatif_view(EQUAL_ANSWERS, replace(EQUAL_ANSWERS, "Health", 4L), XM, LOWC)
  expect_identical(eq, whatif_view(EQUAL_ANSWERS, replace(EQUAL_ANSWERS, "Health", 4L), XM, LOWC))
  expect_equal(nrow(eq$after), 3)
})

test_that("stability_summary_view gives one label, one headline and bars from the existing engines", {
  ss <- stability_summary_view(ORIG, XM, DATA, META, LOWC)
  rec <- compute_recommendation(ORIG, XM, LOWC)
  rb <- robustness_analysis(rec$X, answers = ORIG, preference = "ratings", exact = TRUE, p_change = 0.25)
  expect_equal(ss$top1_share, rb$baseline$top1_prop)
  expect_equal(ss$label, if (ss$top1_share >= 0.9) "Holds up" else if (ss$top1_share >= 0.5) "Usually holds" else "Close call")
  expect_equal(ss$label, "Close call")
  expect_match(ss$headline, "Delhi stays your #1 in 48% of the answer variations")
  expect_true(all(top_k(rec$topsis, 3)$State %in% ss$places$State))
  expect_false(is.unsorted(ss$places$Rank))
  expect_equal(ss$places$Top3_Share, rb$summary$Top3_Prop[match(ss$places$State, rb$summary$State)])
  expect_lte(nrow(ss$places), 5)
  expect_false(grepl("probab", paste(ss$headline, ss$sentence), ignore.case = TRUE))
  expect_match(ss$caveat, "not the chance")
  hold <- stability_summary_view(setNames(rep(5L, 7), CRITERIA), XM, DATA, META, LOWC)
  expect_true(hold$label %in% c("Holds up", "Usually holds", "Close call"))
  ex <- stability_summary_view(ORIG, XM, DATA, META, LOWC, exclude_low = TRUE)
  expect_false(any(ex$places$State %in% LOWC))
  expect_false(identical(stability_summary_view(ORIG, XM, DATA, META, LOWC, p_change = 0.5)$top1_share, ss$top1_share))
})

test_that("comparison_bars_view marks leaders, near-ties and coverage consistently with the scores", {
  rec <- compute_recommendation(ORIG, XM, LOWC)
  st <- c("Delhi", "Kerala", "Chandigarh")
  cb <- comparison_bars_view(rec, st, DATA, META, LOWC)
  expect_equal(nrow(cb$bars), 21)
  expect_equal(unique(cb$bars$Criterion), weights_table(rec$ahp)$Criterion)
  expect_equal(cb$bars$Score, unname(XM[cbind(cb$bars$State, cb$bars$Criterion)]))
  for (cn in unique(cb$bars$Criterion)) {
    x <- cb$bars[cb$bars$Criterion == cn, ]
    expect_equal(x$Tie[1], max(x$Score) - min(x$Score) < NEAR_TIE_SCORE)
    if (any(x$Leads)) {
      s <- sort(x$Score, decreasing = TRUE)
      expect_gte(s[1] - s[2], NEAR_TIE_SCORE)
      expect_equal(x$State[x$Leads], x$State[which.max(x$Score)])
    }
  }
  # Health: Delhi 0.89 vs Kerala 0.87 is within the near-tie margin, so nobody "leads".
  expect_false(any(cb$bars$Leads[cb$bars$Criterion == "Health"]))
  expect_true(any(grepl("nearly tied overall", cb$observations)))
  expect_equal(cb$bars$Flag[cb$bars$State == "Chandigarh" & cb$bars$Criterion == "Housing"], "few indicators")
  expect_match(cb$badges[["Chandigarh"]], "^Limited data:")
  expect_equal(cb$badges[["Kerala"]], "")
  expect_false(any(grepl("Pareto|dominat", cb$observations)))
})

test_that("coverage badges and place reasons come from the data-quality functions", {
  expect_equal(coverage_badge(DATA, META, "Lakshadweep"), "Limited data: Connectivity estimated; Health, Housing partial")
  expect_equal(coverage_badge(DATA, META, "Kerala"), "")
  expect_match(coverage_badge(DATA, META, "Telangana"), "^Data gaps: Connectivity partial")
  rec <- compute_recommendation(ORIG, XM, LOWC)
  pr <- place_reasons(rec, DATA, META, "Kerala")
  expect_equal(pr, recommendation_view(ORIG, XM, DATA, META, LOWC)$reasons[["Kerala"]])
  expect_match(pr$Evidence[pr$Criterion == "Health"], "Immunisation coverage")
})

test_that("tradeoff_chart_data uses the real scores, ranks and axes", {
  rec <- compute_recommendation(ORIG, XM, LOWC)
  cd <- tradeoff_chart_data(rec, "Safety", "Economy", DATA, META, c("Kerala", "Goa"), LOWC)
  expect_equal(nrow(cd$points), 36)
  expect_equal(cd$points$X, unname(XM[cd$points$State, "Safety"]))
  expect_equal(cd$points$Y, unname(XM[cd$points$State, "Economy"]))
  res <- rec$topsis$results
  expect_equal(cd$points$Rank, res$Rank[match(cd$points$State, res$State)])
  expect_equal(cd$points$Match, round(res$Closeness[match(cd$points$State, res$State)], 2))
  expect_equal(sort(cd$points$State[cd$points$Top3]), sort(top_k(rec$topsis, 3)$State))
  expect_setequal(cd$points$State[cd$points$Frontier], tradeoff_view(rec, "Safety", "Economy")$frontier$State)
  # Swapping or changing an axis changes only that coordinate.
  sw <- tradeoff_chart_data(rec, "Economy", "Safety", DATA, META, c("Kerala", "Goa"), LOWC)
  expect_equal(sw$points$X, cd$points$Y); expect_equal(sw$points$Y, cd$points$X)
  ch <- tradeoff_chart_data(rec, "Safety", "Health", DATA, META, c("Kerala", "Goa"), LOWC)
  expect_equal(ch$points$X, cd$points$X)
  expect_equal(ch$points$Y, unname(XM[ch$points$State, "Health"]))
  expect_equal(ch$selected, cd$selected)                           # selection kept
  expect_match(cd$x_title, "^Safety \\(relative score\\)")
  expect_error(tradeoff_chart_data(rec, "Safety", "Safety", DATA, META), "two different")
  expect_error(tradeoff_chart_data(rec, "Safety", "Wealth", DATA, META), "two criteria")
})

test_that("tradeoff_chart_data cards: selection order, positions, observation and warnings", {
  rec <- compute_recommendation(ORIG, XM, LOWC)
  cd <- tradeoff_chart_data(rec, "Safety", "Economy", DATA, META, c("Lakshadweep", "Goa"), LOWC)
  expect_equal(cd$cards$State, c("Lakshadweep", "Goa"))
  expect_equal(cd$cards$Pos_X, unname(rank(-XM[, "Safety"], ties.method = "min")[c("Lakshadweep", "Goa")]))
  expect_equal(cd$cards$Rank, rec$topsis$results$Rank[match(cd$cards$State, rec$topsis$results$State)])
  expect_match(cd$cards$Observation[1], "top third on both Safety and Economy. No other place scores at least as well on both.")
  expect_match(cd$cards$Observation[2], "^Stronger on Economy")
  expect_match(cd$cards$Warning[1], "^Limited data:")
  expect_equal(cd$cards$Warning[2], "")
  # The axis criterion itself is estimated for Lakshadweep on Connectivity.
  ce <- tradeoff_chart_data(rec, "Connectivity", "Health", DATA, META, "Lakshadweep", LOWC)
  expect_equal(ce$cards$Warning, "Connectivity score is estimated, not measured. Health is based on only some of its indicators.")
  expect_true(ce$points$Estimated[ce$points$State == "Lakshadweep"])
  expect_false(grepl("Pareto|dominat", paste(cd$cards$Observation, collapse = " ")))
})

test_that("tradeoff_chart_data handles empty, invalid, too many and excluded selections", {
  rec <- compute_recommendation(ORIG, XM, LOWC)
  empty <- tradeoff_chart_data(rec, "Health", "Education", DATA, META, character(0), LOWC)
  expect_equal(nrow(empty$cards), 0); expect_false(any(empty$points$Selected)); expect_equal(nrow(empty$points), 36)
  expect_equal(nrow(tradeoff_chart_data(rec, "Health", "Education", DATA, META, c("Atlantis", "", NA), LOWC)$cards), 0)
  four <- tradeoff_chart_data(rec, "Health", "Education", DATA, META, c("Goa", "Kerala", "Punjab", "Bihar"), LOWC)
  expect_equal(four$selected, c("Goa", "Kerala", "Punjab"))
  ex <- compute_recommendation(ORIG, XM, LOWC, exclude_low = TRUE)
  ce <- tradeoff_chart_data(ex, "Health", "Education", DATA, META, c("Lakshadweep", "Goa"), LOWC)
  expect_equal(nrow(ce$points), 32)
  expect_equal(ce$selected, "Goa")
  expect_equal(ce$excluded_selected, "Lakshadweep")
  expect_equal(ce$cards$Rank, ex$topsis$results$Rank[ex$topsis$results$State == "Goa"])
  expect_match(ce$note, "among the 32 places ranked")
})

test_that("tradeoff_chart_data never draws a missing score as zero", {
  rec <- compute_recommendation(ORIG, XM, LOWC)
  rec$X["Goa", "Safety"] <- NA                         # simulate a missing criterion score
  cd <- tradeoff_chart_data(rec, "Safety", "Economy", DATA, META, "Goa", LOWC)
  expect_equal(cd$not_plotted, "Goa")
  expect_false("Goa" %in% cd$points$State)
  expect_equal(nrow(cd$points), 35)
  expect_equal(nrow(cd$cards), 0)
  html <- htmltools::renderTags(render_tradeoff_plot(cd))$html
  expect_true(grepl("Not plotted (missing score): Goa.", html, fixed = TRUE))
})

test_that("toggle_place adds, removes and caps the selection", {
  v <- DATA$State
  expect_equal(toggle_place(c("Goa", "Kerala"), "Punjab", v), c("Goa", "Kerala", "Punjab"))
  expect_equal(toggle_place(c("Goa", "Kerala"), "Goa", v), "Kerala")                 # deselect
  expect_equal(toggle_place(c("Goa", "Kerala", "Punjab"), "Bihar", v), c("Kerala", "Punjab", "Bihar"))
  expect_equal(toggle_place(character(0), "Goa", v), "Goa")
  expect_equal(toggle_place(NULL, "Goa", v), "Goa")
  expect_equal(toggle_place(c("Goa"), "Atlantis", v), "Goa")                          # invalid click ignored
  expect_equal(toggle_place(c("Goa", "Atlantis"), NA, v), "Goa")
})

test_that("resolve_axes never returns two identical criteria", {
  expect_equal(resolve_axes("Safety", "Economy"), c("Safety", "Economy"))
  expect_equal(resolve_axes("Economy", "Economy", c("Safety", "Economy")), c("Economy", "Safety"))   # x changed: swap
  expect_equal(resolve_axes("Safety", "Safety", c("Safety", "Economy")), c("Economy", "Safety"))     # y changed: swap
  expect_equal(resolve_axes("Health", "Health"), c("Health", "Education"))
  for (a in CRITERIA) for (b in CRITERIA) expect_false(do.call(identical, as.list(resolve_axes(a, b, c("Safety", "Economy")))))
})
