# The trade-off explorer chart (R/atlas.R) drawn from tradeoff_chart_data().
ORIG_A <- setNames(c(4L, 3L, 5L, 2L, 4L, 1L, 3L), CRITERIA)

test_that("the chart draws one focusable point per ranked place at its real scores", {
  rec <- compute_recommendation(ORIG_A, XM, LOWC)
  cd <- tradeoff_chart_data(rec, "Safety", "Economy", DATA, META, c("Goa", "Kerala"), LOWC)
  html <- htmltools::renderTags(render_tradeoff_plot(cd))$html
  count <- function(p) lengths(regmatches(html, gregexpr(p, html, fixed = TRUE)))[1]
  expect_equal(count('class="ilm-state tc-pt'), 36)
  expect_equal(count('data-click-input="cmp_click"'), 36)
  expect_equal(count("is-selected"), 2)
  expect_equal(count("is-top"), 2)                 # Delhi and Chandigarh (Kerala is selected)
  expect_equal(count('aria-pressed="true"'), 2)
  expect_true(grepl("Goa, rank 8: Safety 0.47, Economy 0.52", html, fixed = TRUE))
  expect_true(grepl("Safety (relative score) → better", html, fixed = TRUE))
  expect_true(grepl("Economy (relative score) → better", html, fixed = TRUE))
  # Pixel positions follow the scores on a fixed 0-1 scale (higher = right / up).
  cx <- function(st) as.numeric(sub('.*cx="([0-9.]+)".*', "\\1", regmatches(html, regexpr(sprintf('<circle cx="[0-9.]+"[^>]*data-state="%s"', st), html))))
  expect_gt(cx("Lakshadweep"), cx("Goa"))          # Safety 0.97 vs 0.47
  expect_false(grepl("Pareto", html))
  # Dashed outlines only for limited or estimated data.
  expect_equal(count('stroke-dasharray="2.5 2"'), sum(cd$points$Estimated | cd$points$Partial | cd$points$Limited))
})

test_that("labels go on the top 3 and selected places only, without overlapping", {
  rec <- compute_recommendation(ORIG_A, XM, LOWC)
  cd <- tradeoff_chart_data(rec, "Health", "Education", DATA, META, c("Punjab"), LOWC)
  html <- htmltools::renderTags(render_tradeoff_plot(cd))$html
  expect_equal(lengths(regmatches(html, gregexpr('class="tc-label"', html, fixed = TRUE)))[1], 4)
  lab <- cd$points[cd$points$Top3 | cd$points$Selected, ]
  txt <- paste0(lab$Short, ifelse(lab$Top3, sprintf(" #%d", lab$Rank), ""))
  pos <- place_labels(64 + lab$X * 594, 412 - lab$Y * 382, txt, 680, 64, 22, 30, 58, 470)
  w <- 6.4 * nchar(txt) + 4
  left <- ifelse(pos$anchor == "start", pos$x, pos$x - w)
  for (i in seq_len(nrow(pos))) for (j in seq_len(nrow(pos))) if (i < j) {
    expect_true(left[i] + w[i] < left[j] || left[j] + w[j] < left[i] || abs(pos$y[i] - pos$y[j]) >= 14)
  }
})

test_that("the chart follows the exclusion switch and its CSS helper is self-contained", {
  rec <- compute_recommendation(ORIG_A, XM, LOWC, exclude_low = TRUE)
  cd <- tradeoff_chart_data(rec, "Health", "Education", DATA, META, character(0), LOWC)
  html <- htmltools::renderTags(render_tradeoff_plot(cd))$html
  expect_equal(lengths(regmatches(html, gregexpr('class="ilm-state tc-pt', html, fixed = TRUE)))[1], 32)
  expect_false(grepl('data-state="Lakshadweep"', html, fixed = TRUE))
  expect_true(grepl(".tc-chart .tc-pt", as.character(atlas_assets()), fixed = TRUE))
})
