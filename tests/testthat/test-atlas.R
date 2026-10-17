# The Trade-off Atlas drawing (R/atlas.R) on top of pareto_front().
test_that("the Atlas draws one focusable point per State/UT with the real Pareto set", {
  rec <- compute_recommendation(EQUAL_ANSWERS, XM, LOWC)
  html <- htmltools::renderTags(render_atlas(rec$X, rec$topsis$results, "Safety", "Economy", LOWC, "Kerala"))$html
  count <- function(p) lengths(regmatches(html, gregexpr(p, html, fixed = TRUE)))[1]
  front <- pareto_front(rec$X, c("Safety", "Economy"))
  expect_equal(count('class="ilm-state atlas-pt'), 36)
  expect_equal(count("is-efficient"), sum(front$Efficient))
  expect_equal(count('fill="#4B3FA8"'), sum(front$Efficient))     # coloured without any stylesheet
  expect_equal(count("ilm-selected"), 1)
  expect_true(grepl("Kerala: Safety 0.38, Economy 0.67", html, fixed = TRUE))
  expect_true(grepl("Pareto-efficient on Safety and Economy: Chandigarh, Ladakh, Lakshadweep, Nagaland, Tripura.", html, fixed = TRUE))
  expect_error(render_atlas(rec$X, rec$topsis$results, "Safety", "Safety"), "two different")
})

test_that("the Atlas follows the low-confidence switch and its CSS helper is self-contained", {
  rec <- compute_recommendation(EQUAL_ANSWERS, XM, LOWC, exclude_low = TRUE)
  html <- htmltools::renderTags(render_atlas(rec$X, rec$topsis$results, "Health", "Education", LOWC))$html
  expect_equal(lengths(regmatches(html, gregexpr('class="ilm-state atlas-pt', html, fixed = TRUE)))[1], 32)
  expect_false(grepl('data-state="Lakshadweep"', html, fixed = TRUE))
  expect_true(grepl(".atlas .ilm-selected", as.character(atlas_assets()), fixed = TRUE))
})
