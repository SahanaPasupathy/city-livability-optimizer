# ==============================================================================
# Trade-off explorer: two criteria, one point per State/UT (Compare tab)
# ------------------------------------------------------------------------------
# Plain SVG (no extra packages), like the map. Draws the result of
# tradeoff_chart_data() (R/view_models.R); nothing is computed here except
# pixel positions and label placement. Points carry data-state / data-tip like
# the map shapes, so the map's tooltip script (india_map_assets() in R/map.R)
# shows their tooltips; `data-click-input` makes a click (or Enter) send the
# place to that Shiny input instead of selecting it on the map. Colours are set
# on the SVG elements, so the chart needs no stylesheet; atlas_assets() only
# adds hover and focus styles. Class names are kept so a redesign can restyle it.
# ==============================================================================

# Short labels for long names, used only on the chart.
SHORT_NAMES <- c("Dadra and Nagar Haveli and Daman and Diu" = "DNH & DD",
                 "Jammu and Kashmir" = "J&K", "Andhra Pradesh" = "Andhra P.",
                 "Arunachal Pradesh" = "Arunachal P.", "Himachal Pradesh" = "Himachal P.",
                 "Madhya Pradesh" = "Madhya P.", "Uttar Pradesh" = "Uttar P.")
short_name <- function(x) ifelse(x %in% names(SHORT_NAMES), SHORT_NAMES[x], x)

# Point styles: other places small and soft, your top 3 in the brand colour,
# selected places larger in amber. Dashed outline = limited or estimated data.
TC_STYLE <- list(other = list(r = 4.5, fill = "#C7CDF3", stroke = "#ffffff", sw = 1),
                 top = list(r = 6.5, fill = "#4B3FA8", stroke = "#ffffff", sw = 1.5),
                 selected = list(r = 8, fill = "#F59E0B", stroke = "#1f2937", sw = 2))

#' Greedy placement of direct labels so they do not overlap (approximate text
#' boxes). Tries right of the point, then left, then shifted up or down.
#' @return data.frame(x, y, anchor) in pixels, one row per label.
place_labels <- function(px, py, text, W, L, R, Tp, B, H) {
  wbox <- 6.4 * nchar(text) + 4; hbox <- 14
  placed <- list()
  overlaps <- function(x0, y0, w) any(vapply(placed, function(b) {
    !(x0 + w < b[1] || b[1] + b[3] < x0 || y0 + hbox < b[2] || b[2] + hbox < y0)
  }, logical(1)))
  out <- data.frame(x = numeric(length(px)), y = numeric(length(px)), anchor = character(length(px)), stringsAsFactors = FALSE)
  for (i in order(py)) {
    best <- NULL
    for (dy in c(0, -14, 14, -28, 28)) {
      for (side in c("right", "left")) {
        x0 <- if (side == "right") px[i] + 10 else px[i] - 10 - wbox[i]
        y0 <- py[i] - 10 + dy
        if (x0 < L || x0 + wbox[i] > W - 4 || y0 < 2 || y0 + hbox > H - B) next
        if (!overlaps(x0, y0, wbox[i])) { best <- c(x0, y0, side); break }
      }
      if (!is.null(best)) break
    }
    if (is.null(best)) best <- c(px[i] + 10, py[i] - 10, "right")
    x0 <- as.numeric(best[1]); y0 <- as.numeric(best[2])
    placed[[length(placed) + 1]] <- c(x0, y0, wbox[i])
    out[i, ] <- list(if (best[3] == "right") x0 else x0 + wbox[i], y0 + 11, if (best[3] == "right") "start" else "end")
  }
  out
}

#' Draw the trade-off explorer chart.
#'
#' @param cd result of tradeoff_chart_data().
#' @param click_input Shiny input that receives the name of a clicked point.
#' @return htmltools tag: the chart, a legend and a note on places not plotted.
render_tradeoff_plot <- function(cd, click_input = "cmp_click") {
  tag <- htmltools::tag
  W <- 680; H <- 470; L <- 64; R <- 22; Tp <- 30; B <- 58
  px <- function(v) L + v * (W - L - R)
  py <- function(v) H - B - v * (H - Tp - B)
  p <- cd$points

  grid <- lapply(c(0, 0.25, 0.5, 0.75, 1), function(v) {
    tag("g", list(class = "tc-grid",
                  tag("line", list(x1 = px(0), x2 = px(1), y1 = py(v), y2 = py(v), stroke = "#EEF0F6")),
                  tag("line", list(x1 = px(v), x2 = px(v), y1 = py(0), y2 = py(1), stroke = "#EEF0F6")),
                  tag("text", list(x = px(0) - 8, y = py(v), class = "tc-tick", `font-size` = 11, fill = "#9CA3AF",
                                   `text-anchor` = "end", `dominant-baseline` = "central", format(v))),
                  tag("text", list(x = px(v), y = py(0) + 17, class = "tc-tick", `font-size` = 11, fill = "#9CA3AF",
                                   `text-anchor` = "middle", format(v)))))
  })
  axes <- list(
    tag("line", list(x1 = px(0), x2 = px(1), y1 = py(0), y2 = py(0), stroke = "#9CA3AF")),
    tag("line", list(x1 = px(0), x2 = px(0), y1 = py(0), y2 = py(1), stroke = "#9CA3AF")),
    tag("text", list(x = px(0.5), y = H - 14, class = "tc-axis", `font-size` = 13, `font-weight` = 700,
                     fill = "#1f2937", `text-anchor` = "middle", paste(cd$x_title, "→ better"))),
    tag("text", list(x = 18, y = py(0.5), class = "tc-axis", `font-size` = 13, `font-weight` = 700,
                     fill = "#1f2937", `text-anchor` = "middle", transform = sprintf("rotate(-90 18 %s)", py(0.5)),
                     paste(cd$y_title, "→ better"))),
    tag("text", list(x = px(1), y = py(1) - 12, class = "tc-corner", `font-size` = 11.5, fill = "#6b7280",
                     `text-anchor` = "end", "Better on both ↗")))

  kind <- ifelse(p$Selected, "selected", ifelse(p$Top3, "top", "other"))
  draw_order <- order(match(kind, c("other", "top", "selected")), -p$Rank)
  pts <- lapply(draw_order, function(i) {
    s <- TC_STYLE[[kind[i]]]
    flag <- p$Estimated[i] || p$Partial[i] || p$Limited[i]
    warn <- c(if (p$Estimated[i]) "Score estimated, not measured" else if (p$Partial[i]) "Some indicators missing",
              if (p$Limited[i]) "Limited data coverage")
    tip <- sprintf(paste0("<div class='ilm-title'>%s <em>#%d</em></div><div class='ilm-sub'>Relative match %.2f of %d places</div>",
                          "<div class='ilm-row'><span>%s</span><b>%.2f</b></div><div class='ilm-row'><span>%s</span><b>%.2f</b></div>%s",
                          "<div class='ilm-sub'>%s</div>"),
                   htmltools::htmlEscape(p$State[i]), p$Rank[i], p$Match[i], cd$n_ranked, cd$x, p$X[i], cd$y, p$Y[i],
                   if (length(warn)) sprintf("<div class='ilm-warn'>&#9888; %s</div>", paste(warn, collapse = "; ")) else "",
                   if (p$Selected[i]) "Click to remove from your comparison" else "Click to add to your comparison")
    tag("circle", list(cx = sprintf("%.1f", px(p$X[i])), cy = sprintf("%.1f", py(p$Y[i])), r = s$r,
                       fill = s$fill, stroke = if (flag) "#374151" else s$stroke, `stroke-width` = s$sw,
                       `stroke-dasharray` = if (flag) "2.5 2" else NULL,
                       class = paste("ilm-state tc-pt", paste0("is-", kind[i])), `data-state` = p$State[i],
                       `data-tip` = tip, `data-click-input` = click_input, tabindex = "0", role = "button",
                       `aria-pressed` = if (p$Selected[i]) "true" else "false",
                       `aria-label` = sprintf("%s, rank %d: %s %.2f, %s %.2f%s", p$State[i], p$Rank[i], cd$x, p$X[i], cd$y, p$Y[i],
                                              if (length(warn)) paste0("; ", paste(warn, collapse = "; ")) else "")))
  })

  lab <- p[p$Top3 | p$Selected, ]
  labels <- NULL
  if (nrow(lab)) {
    txt <- paste0(lab$Short, ifelse(lab$Top3, sprintf(" #%d", lab$Rank), ""))
    pos <- place_labels(px(lab$X), py(lab$Y), txt, W, L, R, Tp, B, H)
    labels <- lapply(seq_len(nrow(lab)), function(i) {
      tag("text", list(x = sprintf("%.1f", pos$x[i]), y = sprintf("%.1f", pos$y[i]), class = "tc-label",
                       `font-size` = 11.5, `font-weight` = if (lab$Selected[i]) 800 else 600,
                       fill = if (lab$Selected[i]) "#92400E" else "#1f2937", stroke = "#ffffff", `stroke-width` = 3,
                       `paint-order` = "stroke", `text-anchor` = pos$anchor[i], `pointer-events` = "none",
                       `aria-hidden` = "true", txt[i]))
    })
  }

  svg <- tag("svg", list(viewBox = sprintf("0 0 %d %d", W, H), class = "tc-svg", role = "group",
                         `font-family` = "system-ui, -apple-system, 'Segoe UI', Roboto, Arial, sans-serif",
                         `aria-label` = sprintf("Scatter plot of %s against %s for %d States/UTs. Each point can be focused and selected with Enter.",
                                                cd$y, cd$x, nrow(p)),
                         grid, axes, tag("g", pts), tag("g", labels)))
  swatch <- function(fill, stroke, text, dash = FALSE) {
    htmltools::tags$span(class = "tc-key",
                         htmltools::tags$span(style = sprintf("width:12px;height:12px;border-radius:50%%;display:inline-block;background:%s;border:1.5px %s %s",
                                                              fill, if (dash) "dashed" else "solid", stroke)), text)
  }
  htmltools::tags$div(
    class = "tc-chart",
    svg,
    htmltools::tags$div(class = "tc-legend",
                        swatch(TC_STYLE$top$fill, TC_STYLE$top$fill, "Your top 3"),
                        swatch(TC_STYLE$selected$fill, TC_STYLE$selected$stroke, "Selected"),
                        swatch(TC_STYLE$other$fill, TC_STYLE$other$fill, "Other places"),
                        swatch("#ffffff", "#374151", "Limited or estimated data", dash = TRUE)),
    if (length(cd$not_plotted)) htmltools::tags$p(class = "note", sprintf("Not plotted (missing score): %s.", paste(cd$not_plotted, collapse = ", "))))
}

#' Hover, focus and selection styles for the chart (include once per page,
#' next to india_map_assets(), whose script provides tooltips and clicks).
atlas_assets <- function() {
  htmltools::tags$style(htmltools::HTML(paste0(
    ".tc-svg{width:100%;max-width:680px;height:auto;display:block;margin:0 auto}",
    ".tc-chart .tc-pt{cursor:pointer;transition:r .12s}",
    ".tc-chart .ilm-hover,.tc-chart .tc-pt:focus{stroke:#111827 !important;stroke-width:2.4px !important;outline:none}",
    ".tc-legend{display:flex;flex-wrap:wrap;gap:6px 16px;justify-content:center;font-size:12.5px;color:#4b5563;margin-top:4px}",
    ".tc-key{display:inline-flex;align-items:center;gap:6px}")))
}
