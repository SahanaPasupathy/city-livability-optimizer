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
