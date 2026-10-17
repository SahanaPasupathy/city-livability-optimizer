# ==============================================================================
# Livability Trade-off Atlas: two criteria, one point per State/UT
# ------------------------------------------------------------------------------
# Plain SVG (no extra packages), like the map. Points carry the same data-state /
# data-tip attributes as the map shapes, so the map's tooltip and selection
# script (india_map_assets() in R/map.R) works here too and a State/UT selected
# in one view is selected in the other. Colours are set on the SVG elements
# themselves (original purple palette) and atlas_assets() adds hover/selection
# styles, so the chart needs nothing from the app's stylesheet. The class names
# are kept so a redesign can restyle it with CSS.
# ==============================================================================

# Short labels for long names, used only on the chart.
SHORT_NAMES <- c("Dadra and Nagar Haveli and Daman and Diu" = "DNH & DD",
                 "Jammu and Kashmir" = "J&K", "Andhra Pradesh" = "Andhra P.",
                 "Arunachal Pradesh" = "Arunachal P.", "Himachal Pradesh" = "Himachal P.",
                 "Madhya Pradesh" = "Madhya P.", "Uttar Pradesh" = "Uttar P.")
short_name <- function(x) ifelse(x %in% names(SHORT_NAMES), SHORT_NAMES[x], x)

#' Draw the Trade-off Atlas.
#'
#' @param X matrix of criterion scores for the States/UTs in the current ranking.
#' @param results topsis()$results for the same States/UTs (Rank, Closeness).
#' @param xcrit,ycrit the two criteria to plot (different, both columns of X).
#' @param low_confidence names of low-confidence States/UTs (dashed outline).
#' @param selected optional State/UT drawn as selected.
#' @return htmltools tag: the chart, its legend and a text list for screen readers.
render_atlas <- function(X, results, xcrit, ycrit, low_confidence = character(0), selected = NULL) {
  if (identical(xcrit, ycrit)) stop("Choose two different criteria.", call. = FALSE)
  front <- pareto_front(X, c(xcrit, ycrit))
  tag <- htmltools::tag
  W <- 640; H <- 440; L <- 64; R <- 24; Tp <- 26; B <- 54
  px <- function(v) L + v * (W - L - R)
  py <- function(v) H - B - v * (H - Tp - B)
  top_state <- results$State[which.min(results$Rank)]
  rank_of <- setNames(results$Rank, results$State)
  n <- nrow(X)

  grid <- lapply(c(0, 0.25, 0.5, 0.75, 1), function(v) {
    tag("g", list(class = "atlas-grid",
                  tag("line", list(x1 = px(0), x2 = px(1), y1 = py(v), y2 = py(v), stroke = "#E5E7EB")),
                  tag("line", list(x1 = px(v), x2 = px(v), y1 = py(0), y2 = py(1), stroke = "#E5E7EB")),
                  tag("text", list(x = px(0) - 8, y = py(v), class = "atlas-tick atlas-tick-y", `font-size` = 11,
                                   fill = "#6b7280", `text-anchor` = "end", `dominant-baseline` = "central", format(v))),
                  tag("text", list(x = px(v), y = py(0) + 18, class = "atlas-tick atlas-tick-x", `font-size` = 11,
                                   fill = "#6b7280", `text-anchor` = "middle", format(v)))))
  })
  axes <- list(
    tag("text", list(x = px(0.5), y = H - 10, class = "atlas-axis", `font-size` = 12.5, `font-weight` = 600,
                     fill = "#1f2937", `text-anchor` = "middle", sprintf("%s score \u2192 better", xcrit))),
    # x = 24 leaves room for the glyphs of the rotated label, also at the larger phone font size.
    tag("text", list(x = 24, y = py(0.5), class = "atlas-axis", `font-size` = 12.5, `font-weight` = 600,
                     fill = "#1f2937", `text-anchor` = "middle",
                     transform = sprintf("rotate(-90 24 %s)", py(0.5)), sprintf("%s score \u2192 better", ycrit))))

  # Frontier as a staircase through the efficient points (left to right).
  eff <- pareto_frontier_order(X, front)
  frontier <- if (length(eff) >= 2) {
    d <- sprintf("M%.1f %.1f", px(X[eff[1], xcrit]), py(X[eff[1], ycrit]))
    for (k in 2:length(eff)) {
      d <- paste0(d, sprintf(" H%.1f V%.1f", px(X[eff[k], xcrit]), py(X[eff[k], ycrit])))
    }
    tag("path", list(d = d, class = "atlas-frontier", fill = "none", stroke = "#4B3FA8", `stroke-width` = 1.5,
                     `stroke-dasharray` = "4 3", opacity = 0.6))
  }

  status <- setNames(front$Status, front$State)
  pts <- lapply(order(status[rownames(X)] == "efficient"), function(i) {   # efficient drawn last (on top)
    st <- rownames(X)[i]
    x <- X[st, xcrit]; y <- X[st, ycrit]
    if (is.na(x) || is.na(y)) return(NULL)
    eff_st <- status[[st]] == "efficient"
    low <- st %in% low_confidence
    cls <- trimws(paste("ilm-state atlas-pt", if (eff_st) "is-efficient" else "is-dominated",
                        if (low) "is-low", if (!is.null(selected) && identical(st, selected)) "ilm-selected"))
    label <- sprintf("%s: %s %.2f, %s %.2f; %s; rank %d of %d%s", st, xcrit, x, ycrit, y,
                     if (eff_st) "Pareto-efficient on these two" else "dominated on these two",
                     rank_of[[st]], n, if (low) "; low data confidence" else "")
    tip <- sprintf(paste0("<div class='ilm-title'>%s</div><div class='ilm-sub'>Rank <b>%d</b> of %d &middot; %s</div>",
                          "<div class='ilm-row'><span>%s</span><b>%.2f</b></div><div class='ilm-row'><span>%s</span><b>%.2f</b></div>%s"),
                   htmltools::htmlEscape(st), rank_of[[st]], n,
                   if (eff_st) "Pareto-efficient" else "dominated", xcrit, x, ycrit, y,
                   if (low) "<div class='ilm-warn'>&#9888; Low data confidence</div>" else "")
    tag("circle", list(cx = sprintf("%.1f", px(x)), cy = sprintf("%.1f", py(y)), r = if (eff_st) 6.5 else 5.5,
                       fill = if (eff_st) "#4B3FA8" else "#ffffff", stroke = if (eff_st) "#ffffff" else "#9CA3AF",
                       `stroke-width` = 1.5, `stroke-dasharray` = if (low) "2.5 2" else NULL,
                       class = cls, `data-state` = st, `data-tip` = tip, tabindex = "0", role = "button",
                       `aria-label` = label))
  })
  top_ring <- tag("circle", list(cx = sprintf("%.1f", px(X[top_state, xcrit])),
                                 cy = sprintf("%.1f", py(X[top_state, ycrit])), r = 11,
                                 class = "atlas-top", fill = "none", stroke = "#F59E0B", `stroke-width` = 2,
                                 `pointer-events` = "none", `aria-hidden` = "true"))
  label_states <- unique(c(eff, top_state, selected))
  label_states <- label_states[label_states %in% rownames(X)]
  labels <- lapply(label_states, function(st) {
    x <- px(X[st, xcrit]); y <- py(X[st, ycrit]); right <- x > W - R - 90
    # Labels go above the point, or below it when the point is at the top edge.
    ly <- if (y < Tp + 24) y + 20 else y - 8
    tag("text", list(x = sprintf("%.1f", x + if (right) -10 else 10), y = sprintf("%.1f", ly),
                     class = paste("atlas-label", if (right) "is-right"), `aria-hidden` = "true",
                     `font-size` = 11, `font-weight` = 600, fill = "#1f2937", stroke = "#ffffff", `stroke-width` = 3,
                     `paint-order` = "stroke", `text-anchor` = if (right) "end" else "start", `pointer-events` = "none",
                     paste0(short_name(st), if (st == top_state) " \u2605" else "")))
  })

  svg <- tag("svg", list(viewBox = sprintf("0 0 %d %d", W, H), class = "atlas-svg", role = "group",
                         `aria-label` = sprintf("Scatter plot of %s against %s for %d States/UTs. Each point can be focused and selected.",
                                                ycrit, xcrit, n),
                         grid, axes, frontier, top_ring, tag("g", pts), tag("g", labels)))
  eff_names <- front$State[which(front$Efficient)]
  swatch <- function(style, text) {
    htmltools::tags$span(class = "ilm-key", style = "display:inline-flex;align-items:center;gap:6px;margin-right:14px",
                         htmltools::tags$span(style = paste0("width:12px;height:12px;border-radius:50%;display:inline-block;", style)),
                         text)
  }
  insufficient <- front$State[front$Status == "insufficient data"]
  htmltools::tags$div(
    class = "atlas",
    svg,
    htmltools::tags$div(
      class = "atlas-legend", style = "font-size:13px;color:#4b5563;margin-top:6px",
      swatch("background:#4B3FA8", "Pareto-efficient"),
      swatch("background:#fff;border:1.5px solid #9CA3AF", "Dominated"),
      swatch("border:2px solid #F59E0B", "\u2605 Your #1 match"),
      swatch("background:#fff;border:1.5px dashed #374151", "Low data confidence")),
    htmltools::tags$p(class = "note atlas-efficient",
                      sprintf("Pareto-efficient on %s and %s: %s.", xcrit, ycrit, paste(eff_names, collapse = ", ")),
                      if (length(insufficient)) sprintf(" Not assessed (missing score): %s.", paste(insufficient, collapse = ", "))))
}

#' Hover, focus and selection styles for the Atlas points (include once per
#' page, next to india_map_assets(), whose script provides the tooltips).
atlas_assets <- function() {
  htmltools::tags$style(htmltools::HTML(paste0(
    ".atlas-svg{width:100%;max-width:640px;height:auto;display:block;margin:0 auto}",
    ".atlas .atlas-pt{cursor:pointer}",
    ".atlas .ilm-hover,.atlas .atlas-pt:focus{stroke:#111827 !important;stroke-width:2.4px !important;outline:none}",
    ".atlas .ilm-selected{stroke:#111827 !important;stroke-width:3px !important;stroke-dasharray:none !important}")))
}
