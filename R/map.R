# ==============================================================================
# Interactive India map, coloured by each State/UT's personalised TOPSIS score.
# ------------------------------------------------------------------------------
# The map is drawn as plain SVG (one <path> per State/UT) from outlines stored
# in data/map/india_map_paths.csv, so it needs no extra R packages: only
# htmltools, which is installed together with shiny. Hovering shows the rank and
# the seven criterion scores; clicking selects a State/UT (inside the Shiny app
# the click is also sent to R as input$map_selected).
# Outline source: https://github.com/udit-001/india-maps-data (commit 2884453),
# built with data_extraction/build_map_paths.py.
# ==============================================================================

MAP_PATH <- file.path("data", "map", "india_map_paths.csv")

# Light -> dark ramp: darker = closer to the user's ideal = better match.
MAP_RAMP <- c("#EEF1FB", "#BFC8F5", "#7C8CE6", "#4B3FA8", "#2A1F6B")

#' Read the State/UT outlines.
#' @param path CSV written by data_extraction/build_map_paths.py.
#' @return data.frame(State, Path, Marker, MarkerX, MarkerY, Area, ViewW, ViewH).
load_map_paths <- function(path = MAP_PATH) {
  if (!file.exists(path)) {
    stop("Map file not found at '", path, "'. Put india_map_paths.csv in the ",
         "data/map folder.", call. = FALSE)
  }
  m <- read.csv(path, stringsAsFactors = FALSE)
  need <- c("State", "Path", "Marker", "MarkerX", "MarkerY", "Area", "ViewW", "ViewH")
  if (!all(need %in% names(m))) {
    stop("Map file is missing columns: ", paste(setdiff(need, names(m)), collapse = ", "),
         call. = FALSE)
  }
  m$Marker <- as.logical(m$Marker)
  m
}

#' Map closeness values onto the light-to-dark colour ramp.
#' The lowest closeness gets the lightest colour and the highest the darkest,
#' so the colours always use the full range for the current user.
#' @param closeness numeric vector.
#' @return character vector of hex colours.
closeness_colours <- function(closeness) {
  rng <- range(closeness)
  t <- if (diff(rng) == 0) rep(0.5, length(closeness)) else (closeness - rng[1]) / diff(rng)
  ramp <- grDevices::colorRamp(MAP_RAMP, space = "Lab")
  rgb <- ramp(t)
  grDevices::rgb(rgb[, 1], rgb[, 2], rgb[, 3], maxColorValue = 255)
}

#' Build the hover card (HTML text) for one State/UT.
map_tooltip <- function(state, rank, closeness, scores, low_confidence) {
  bars <- paste(sprintf(
    "<div class='ilm-row'><span>%s</span><span class='ilm-bar'><i style='width:%.0f%%'></i></span><b>%.2f</b></div>",
    names(scores), 100 * scores, scores), collapse = "")
  warn <- if (low_confidence) "<div class='ilm-warn'>Low data confidence</div>" else ""
  sprintf("<div class='ilm-title'>%s <em>#%d</em></div><div class='ilm-sub'>Closeness %.3f</div>%s%s",
          htmltools::htmlEscape(state), rank, closeness, bars, warn)
}

#' The CSS and JavaScript the map needs (include once per page).
#' @return htmltools tag list.
india_map_assets <- function() {
  css <- r"(
.ilm-map{max-width:640px;margin:0 auto;font-family:inherit}
.ilm-map svg{width:100%;height:auto;display:block}
.ilm-map .ilm-state{cursor:pointer;transition:filter .15s,stroke-width .15s}
.ilm-map path.ilm-state{stroke:#fff;stroke-width:1.1;fill-rule:evenodd}
.ilm-map path.ilm-low{stroke:#374151;stroke-width:1.7;stroke-dasharray:5 3}
.ilm-map circle.ilm-state{stroke:#fff;stroke-width:2}
.ilm-map .ilm-hover{filter:brightness(.88);stroke:#111827 !important;stroke-width:2.2 !important}
.ilm-map .ilm-selected{stroke:#111827 !important;stroke-width:3 !important}
.ilm-map .ilm-badge{pointer-events:none;stroke:#fff;stroke-width:2}
.ilm-map .ilm-badge-text{pointer-events:none;font:700 15px sans-serif;fill:#1f2937;text-anchor:middle;dominant-baseline:central}
.ilm-map .ilm-badge-small{font-size:12px}
.ilm-legend{margin:10px auto 0;max-width:420px;font-size:13px;color:#4b5563}
.ilm-legend .ilm-grad{height:12px;border-radius:6px;margin:4px 0}
.ilm-legend .ilm-ends{display:flex;justify-content:space-between}
.ilm-legend .ilm-note{margin-top:6px;font-size:12px;color:#6b7280}
#ilm-tip{position:fixed;display:none;z-index:9999;pointer-events:none;background:#1f2937;color:#f9fafb;
 padding:10px 12px;border-radius:10px;font:13px/1.35 sans-serif;box-shadow:0 6px 20px rgba(0,0,0,.25);min-width:210px}
#ilm-tip .ilm-title{font-weight:700;font-size:14px}#ilm-tip em{font-style:normal;color:#fcd34d;margin-left:4px}
#ilm-tip .ilm-sub{color:#cbd5e1;margin-bottom:6px}
#ilm-tip .ilm-row{display:flex;align-items:center;gap:6px;margin:2px 0}
#ilm-tip .ilm-row span:first-child{width:84px}
#ilm-tip .ilm-bar{flex:1;height:6px;background:#475569;border-radius:3px;overflow:hidden}
#ilm-tip .ilm-bar i{display:block;height:100%;background:#a5b4fc}
#ilm-tip .ilm-row b{width:34px;text-align:right}
#ilm-tip .ilm-warn{margin-top:6px;color:#fca5a5;font-size:12px}
)"
  js <- r"(
(function(){
  if (window.__ilmBound) return; window.__ilmBound = true;
  function tipEl(){var t=document.getElementById('ilm-tip');
    if(!t){t=document.createElement('div');t.id='ilm-tip';document.body.appendChild(t);}return t;}
  function target(e){return e.target.closest?e.target.closest('.ilm-state'):null;}
  function same(name,fn){document.querySelectorAll('.ilm-state').forEach(function(n){
    if(n.getAttribute('data-state')===name)fn(n);});}
  document.addEventListener('mouseover',function(e){var t=target(e);if(!t)return;
    var tip=tipEl();tip.innerHTML=t.getAttribute('data-tip');tip.style.display='block';
    same(t.getAttribute('data-state'),function(n){n.classList.add('ilm-hover');});});
  document.addEventListener('mousemove',function(e){var t=target(e);var tip=tipEl();
    if(!t){return;}var x=e.clientX+16,y=e.clientY+16;
    if(x+tip.offsetWidth>window.innerWidth-8)x=e.clientX-tip.offsetWidth-16;
    if(y+tip.offsetHeight>window.innerHeight-8)y=e.clientY-tip.offsetHeight-16;
    tip.style.left=Math.max(8,x)+'px';tip.style.top=Math.max(8,y)+'px';});
  document.addEventListener('mouseout',function(e){var t=target(e);if(!t)return;
    tipEl().style.display='none';
    same(t.getAttribute('data-state'),function(n){n.classList.remove('ilm-hover');});});
  document.addEventListener('click',function(e){var t=target(e);if(!t)return;
    var name=t.getAttribute('data-state');
    document.querySelectorAll('.ilm-selected').forEach(function(n){n.classList.remove('ilm-selected');});
    same(name,function(n){n.classList.add('ilm-selected');});
    if(window.Shiny&&Shiny.setInputValue){Shiny.setInputValue('map_selected',name,{priority:'event'});}});
})();
)"
  htmltools::tagList(htmltools::tags$style(htmltools::HTML(css)),
                     htmltools::tags$script(htmltools::HTML(js)))
}

#' Draw the interactive India map.
#'
#' @param results data.frame from topsis()$results (State, Closeness, Rank).
#' @param X numeric matrix of the seven criterion scores (rows named by State).
#' @param low_confidence character vector of States/UTs with thin data.
#' @param map_data outline table from load_map_paths().
#' @return htmltools tag (map + legend). Add india_map_assets() to the page once.
render_india_map <- function(results, X, low_confidence = character(0),
                             map_data = load_map_paths()) {
  gap <- c(setdiff(map_data$State, results$State), setdiff(results$State, map_data$State))
  if (length(gap) > 0) {
    stop("State names do not match between the map and the results: ",
         paste(unique(gap), collapse = ", "), call. = FALSE)
  }
  res <- results[match(map_data$State, results$State), ]
  cols <- closeness_colours(res$Closeness)
  view <- sprintf("0 0 %s %s", map_data$ViewW[1], map_data$ViewH[1])
  tag <- htmltools::tag
  
  attrs <- function(i) {
    st <- map_data$State[i]
    list(`data-state` = st,
         `data-tip` = map_tooltip(st, res$Rank[i], res$Closeness[i], X[st, ],
                                  st %in% low_confidence))
  }
  # Draw large States/UTs first so small ones are never hidden underneath.
  draw_order <- order(-map_data$Area)
  shapes <- lapply(draw_order, function(i) {
    cls <- trimws(paste("ilm-state", if (map_data$State[i] %in% low_confidence) "ilm-low"))
    do.call(tag, list("path", c(list(d = map_data$Path[i], fill = cols[i], class = cls),
                                attrs(i))))
  })
  small <- which(map_data$Marker)
  # A small State/UT in the top 3 gets a larger circle so its medal badge can sit
  # exactly in the middle of it, with the closeness colour showing as a ring.
  markers <- lapply(small, function(i) {
    do.call(tag, list("circle", c(list(cx = map_data$MarkerX[i], cy = map_data$MarkerY[i],
                                       r = if (res$Rank[i] <= 3) 16 else 12,
                                       fill = cols[i], class = "ilm-state"),
                                  attrs(i))))
  })
  medal <- c("#FCD34D", "#D1D5DB", "#F59E0B")
  top3 <- which(res$Rank <= 3)
  # Badges are always centred on the State/UT's own anchor point (never offset),
  # so a badge can never appear on top of a neighbouring State/UT.
  badges <- lapply(top3, function(i) {
    cx <- map_data$MarkerX[i]; cy <- map_data$MarkerY[i]
    small_one <- map_data$Marker[i]
    tag("g", list(class = "ilm-badge-group", `data-badge-state` = map_data$State[i],
                  tag("circle", list(cx = cx, cy = cy, r = if (small_one) 10 else 13,
                                     fill = medal[res$Rank[i]], class = "ilm-badge")),
                  tag("text", list(x = cx, y = cy,
                                   class = if (small_one) "ilm-badge-text ilm-badge-small" else "ilm-badge-text",
                                   res$Rank[i]))))
  })
  
  svg <- tag("svg", list(viewBox = view, role = "img",
                         `aria-label` = "Map of India shaded by personalised match",
                         tag("g", shapes), tag("g", markers), tag("g", badges)))
  
  rng <- range(res$Closeness)
  grad <- sprintf("linear-gradient(to right, %s)", paste(MAP_RAMP, collapse = ", "))
  legend <- htmltools::tags$div(
    class = "ilm-legend",
    htmltools::tags$div(class = "ilm-ends",
                        htmltools::tags$span(sprintf("Weaker match (%.2f)", rng[1])),
                        htmltools::tags$span(sprintf("Better match (%.2f)", rng[2]))),
    htmltools::tags$div(class = "ilm-grad", style = paste0("background:", grad)),
    htmltools::tags$div(class = "ilm-note",
                        "Colour = TOPSIS closeness for YOUR priorities (darker = better). Circles mark very small ",
                        "States/UTs; numbered badges mark your top 3; a dashed grey outline means low data confidence. ",
                        "Hover for scores, click to select."))
  htmltools::tags$div(class = "ilm-map", svg, legend)
}

#' Wrap a map in a complete, standalone HTML page.
#' @param map tag returned by render_india_map().
#' @param title page heading.
#' @param note optional line under the heading.
map_page <- function(map, title, note = NULL) {
  htmltools::tagList(
    htmltools::tags$head(htmltools::tags$meta(charset = "utf-8"),
                         htmltools::tags$meta(name = "viewport",
                                              content = "width=device-width, initial-scale=1"),
                         htmltools::tags$title(title), india_map_assets()),
    htmltools::tags$body(
      style = "margin:0;padding:24px 16px;background:#F7F8FD;font-family:system-ui,sans-serif;color:#1f2937",
      htmltools::tags$h2(title, style = "text-align:center;margin:0 0 4px"),
      if (!is.null(note)) htmltools::tags$p(note, style = "text-align:center;margin:0 0 12px;color:#6b7280"),
      map))
}