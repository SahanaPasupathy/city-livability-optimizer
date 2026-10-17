# Score pipeline, cell status, coverage and provenance (R/data_quality.R).

test_that("the pipeline rebuilds every stored criterion score from the raw indicators", {
  b <- build_criterion_scores(DATA, META)
  expect_equal(dim(b$scores), c(36, 7))
  expect_lt(max(abs(b$scores - XM)), 2e-4)          # stored scores have 4 decimals
  expect_equal(unname(b$total), c(3L, 2L, 3L, 3L, 3L, 1L, 2L))
})

test_that("Lakshadweep's Connectivity is the only imputed score and equals the median of the others", {
  b <- build_criterion_scores(DATA, META)
  expect_equal(sum(b$imputed), 1)
  expect_true(b$imputed["Lakshadweep", "Connectivity"])
  others <- b$scores[rownames(b$scores) != "Lakshadweep", "Connectivity"]
  expect_equal(unname(b$scores["Lakshadweep", "Connectivity"]), stats::median(others))
  expect_equal(unname(XM["Lakshadweep", "Connectivity"]), 0.4102, tolerance = 1e-4)
})

test_that("cell status records missing and capped values and keeps raw values untouched", {
  b <- build_criterion_scores(DATA, META)
  expect_equal(sum(b$status == "missing"), sum(is.na(DATA[INDICATORS])))       # 21 cells
  expect_equal(sum(b$status == "capped at 100"),
               sum(DATA$Immunisation_Pct > 100) + sum(DATA$Clean_Cooking_Fuel_Pct > 100))
  expect_equal(unname(b$status["Delhi", "AirQuality_PM25"]), "capped at the 95th percentile")
  ev <- indicator_detail(DATA, META, "Ladakh")
  expect_equal(ev$Raw, unname(unlist(DATA[DATA$State == "Ladakh", INDICATORS])))   # 154.37 stays 154.37
  expect_equal(ev$Status[ev$Indicator == "Clean_Cooking_Fuel_Pct"], "capped at 100")
  expect_true(is.na(ev$Scaled[ev$Indicator == "Health_Worker_Density"]))
})

test_that("scaling follows direction, handles ties and can switch the percentile cap off", {
  up <- scale_indicator(c(1, 2, 3, NA), "higher_better", p = 0)
  down <- scale_indicator(c(1, 2, 3, NA), "lower_better", p = 0)
  expect_equal(up$scaled, c(0, 0.5, 1, NA))
  expect_equal(down$scaled, c(1, 0.5, 0, NA))
  expect_equal(up$status, c("observed", "observed", "observed", "missing"))
  flat <- scale_indicator(c(5, 5, 5), "higher_better")
  expect_equal(flat$scaled, c(0, 0, 0))
  expect_false(any(is.nan(flat$scaled)))
  pct <- scale_indicator(c(50, 120, 90), "higher_better", cap_pct = TRUE, p = 0)
  expect_equal(pct$status[2], "capped at 100")
  expect_equal(pct$scaled, c(0, 1, 0.8))
  b0 <- build_criterion_scores(DATA, META, p = 0)
  expect_false(any(grepl("percentile", b0$status)))
})

test_that("coverage levels reflect indicator availability per criterion", {
  ct <- coverage_table(DATA, META)
  sup <- function(st, cn) ct$Support[ct$State == st & ct$Criterion == cn]
  expect_equal(sup("Chandigarh", "Housing"), "thin")          # 1 of 3
  expect_equal(sup("Delhi", "Housing"), "partial")            # 2 of 3
  expect_equal(sup("Telangana", "Connectivity"), "partial")   # 1 of 2
  expect_equal(sup("Lakshadweep", "Connectivity"), "imputed") # 0 of 2
  expect_true(all(ct$Support[ct$State == "Kerala"] == "full"))
  expect_equal(nrow(ct), 36 * 7)
})

test_that("limited coverage is rule-based and matches the stored flag on the current data", {
  expect_setequal(LIMITED, DATA$State[DATA$Low_Confidence == 1])
  cs <- coverage_summary(DATA, META)
  expect_equal(cs$Missing_Indicators, DATA$Missing_Subindicators)
  expect_false(cs$Limited_Coverage[cs$State == "Puducherry"])          # 2 missing, partial only
  expect_match(cs$Note[cs$State == "Puducherry"], "^Data note: 2 of 17")
  expect_equal(cs$Note[cs$State == "Kerala"], "")
  # A single thin criterion is enough, even with fewer than 3 missing indicators.
  d2 <- DATA
  d2[d2$State == "Kerala", c("Kachha_Houses_Pct", "Rural_Piped_Water_Pct")] <- NA
  expect_true("Kerala" %in% limited_coverage_states(d2, META))
})

test_that("metadata records provenance honestly and criterion descriptions state their limits", {
  expect_true(all(c("public_label", "provenance_status", "interpretation_limits") %in% names(META)))
  untraced <- META$indicator[META$provenance_status == "source not traced"]
  expect_setequal(untraced, c("Education_Score", "Crime_Rate", "Avg_Inflation_PriceStability", "EVs_per_Lakh"))
  expect_true(all(META$provenance_status %in% c("source not traced", "source not recorded")))
  expect_true(all(startsWith(META$source, "not recorded")))          # nothing invented
  info <- setNames(CRITERION_INFO$Not_Measured, CRITERION_INFO$Criterion)
  expect_equal(names(info), CRITERIA)
  expect_match(info[["Housing"]], "Rent")
  expect_match(info[["Connectivity"]], "Public transport")
  expect_match(info[["Economy"]], "Salaries")
  expect_match(info[["Health"]], "Distance to hospitals")
  expect_match(info[["Safety"]], "Personal safety")
  expect_match(CRITERION_INFO$Measures[CRITERION_INFO$Criterion == "Environment"], "PM2.5\\) only")
  expect_equal(criterion_label("Safety"), "Safety (recorded crime)")
})
