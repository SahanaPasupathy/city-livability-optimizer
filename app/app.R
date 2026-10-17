# ==============================================================================
# City Livability Optimizer - Shiny app (Steps 7a-7b)
# ------------------------------------------------------------------------------
# Flow : welcome -> seven flashcard questions (1-5) -> results.
# Calculations are NOT done here: they come from the tested functions in R/
# (AHP weights, TOPSIS ranking, explanations, similar States, map, Pareto
# trade-offs, robustness, what-if scenarios, catalogue, news). This file only
# decides which screen to show, lays out the controls and reacts to clicks.
# Run  : open app/app.R in RStudio and click "Run App" (or shiny::runApp("app")).
# ==============================================================================

library(shiny)
library(bslib)

# ---- Load the project code and data -----------------------------------------
# "Run App" starts with the working directory set to this app/ folder, so the
# project root is one level up.
ROOT <- if (file.exists(file.path("..", "R", "common.R"))) {
  ".."
} else if (file.exists(file.path("R", "common.R"))) {
  "."
} else {
  stop("Cannot find the project's R/ folder. Open the RStudio project first.")
}
for (f in c("common", "ahp", "topsis", "similarity", "explain", "compare", "map", "app_logic", "app_text",
            "report", "sensitivity", "pareto", "atlas", "catalogue", "news", "robustness", "scenario",
            "data_quality", "view_models")) {
  source(file.path(ROOT, "R", paste0(f, ".R")))
}
d          <- load_livability_data(file.path(ROOT, "data", "State_UT_Livability_Dataset_Final.csv"))
X          <- criteria_matrix(d)
STATE_TYPE <- setNames(d$Type, d$State)
MAP_DATA   <- load_map_paths(file.path(ROOT, "data", "map", "india_map_paths.csv"))
META       <- load_indicator_metadata(file.path(ROOT, "data", "indicator_metadata.csv"))
# Places with limited data coverage (R/data_quality.R: 3+ indicators missing, or
# a criterion with fewer than half of its indicators or none). On the current
# data these are the same four places as the dataset's Low_Confidence column.
LOW_CONF   <- limited_coverage_states(d, META)
COVERAGE   <- coverage_summary(d, META)
# Data notes per State/UT (coverage_summary()), shown with the robustness
# results so places scored on thin data are visible.
DATA_FLAGS <- setNames(COVERAGE$Note, COVERAGE$State)
DATA_FLAGS <- DATA_FLAGS[nzchar(DATA_FLAGS)]
# Stability settings: "How sure are you of your answers?" maps to the chance
# that each answer is one point off. The robustness engine enumerates every
# combination exactly (no simulation count, no random seed). These are
# assumptions the user chooses, not estimates of how people answer.
ROBUST_LEVELS <- list(ratings = c(low = 0.10, medium = 0.25, high = 0.50))
# News headlines are refreshed by one background process shared by all sessions.
NEWS_STATE <- new.env()

PRESET_BLURB <- c("Economy, connectivity and housing first",
                  "Education, safety and health first",
                  "Health, environment and safety first")
# Verdicts are positions among the places ranked (see priority_breakdown()).
VERDICT_CLASS <- c("Top third" = "strong", "Middle third" = "moderate",
                   "Bottom third" = "short")
VERDICT_COLOUR <- c("Top third" = "#10B981", "Middle third" = "#F59E0B",
                    "Bottom third" = "#EF4444")

# ---- Small UI helpers --------------------------------------------------------
# A button that sends one click event to the server under input$<id>. The random
# value makes every click count, even when the same button is clicked twice.
ui_button <- function(id, label, class) {
  tags$button(type = "button", class = class,
              onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", id),
              label)
}

welcome_screen <- function() {
  div(class = "screen",
      div(class = "hero",
          div(class = "logo", tags$img(src = "img/logo.svg", alt = "", width = 64, height = 64)),
          tags$h1("City Livability Optimizer"),
          tags$p(class = "lead",
                 "Find States and UTs that may suit your priorities. Answer 7 quick questions and get a personal top 3 out of 36, with reasons, trade-offs and data warnings. Based on State/UT-level indicators, not individual cities."),
          ui_button("start", HTML("Start &rarr;"), "btn-main")),
      div(class = "preset-section",
          tags$h3("...or start from a preset, then adjust it"),
          div(class = "preset-row",
              lapply(seq_along(PERSONAS), function(i) {
                tags$button(type = "button", class = "preset-btn",
                            onclick = sprintf("Shiny.setInputValue('preset_%d', Math.random(), {priority: 'event'})", i),
                            tags$b(names(PERSONAS)[i]), tags$span(PRESET_BLURB[i]))
              }))),
      div(class = "skip-link",
          ui_button("skip_all", "Skip the questions and show the equal-weight ranking", "btn-ghost")),
      tags$details(class = "how",
                   tags$summary("How does this work?"),
                   tags$p("You rate how important seven topics are to you. Your ratings become weights, and every State and UT is ",
                          "ranked by how close it comes to the best place for those weights, using State/UT-level indicators. ",
                          "The method is explained in the app under How it works.")))
}

question_screen <- function(step, answers) {
  q <- QUESTIONS[step, ]
  current <- answers[step]
  buttons <- lapply(1:5, function(v) {
    selected <- !is.na(current) && current == v
    tags$button(type = "button", class = paste("scale-btn", if (selected) "selected"),
                onclick = sprintf("Shiny.setInputValue('answer', {step: %d, value: %d}, {priority: 'event'})", step, v),
                tags$b(v), tags$small(ANSWER_LABELS[v]))
  })
  div(class = "screen fc-wrap",
      div(class = "fc-progress", span(sprintf("Question %d of %d", step, nrow(QUESTIONS))),
          span(sprintf("%.0f%%", 100 * (step - 1) / nrow(QUESTIONS)))),
      div(class = "fc-bar", tags$i(style = sprintf("width:%.0f%%", 100 * (step - 1) / nrow(QUESTIONS)))),
      div(class = "flashcard",
          div(class = "fc-icon", HTML(q$Icon)),
          tags$h2(class = "fc-title", "How important is ", tags$span(q$Label), " to you?"),
          tags$p(class = "fc-hint", q$Hint),
          div(class = "scale", buttons)),
      div(class = "fc-nav",
          ui_button("back", HTML("&larr; Back"), "btn-ghost"),
          if (!is.na(current)) ui_button("keep_next", HTML("Keep this answer &rarr;"), "btn-soft")))
}

why_row <- function(row, n) {
  vclass <- VERDICT_CLASS[[row$Verdict]]
  icon <- QUESTIONS$Icon[QUESTIONS$Criterion == row$Criterion]
  div(class = "why-row",
      div(class = "why-top", span(HTML(icon), " ", row$Criterion),
          span(class = paste("verdict", vclass), row$Verdict)),
      div(class = "why-bar", tags$i(style = sprintf("width:%.0f%%;background:%s", 100 * row$Score,
                                                    VERDICT_COLOUR[[row$Verdict]]))),
      div(class = "why-text", sprintf("%s of %d places ranked", ordinal(row$Position), n)))
}

# One place card (Top 3 and map selection): reasons from place_reasons().
top_card <- function(rec, st) {
  row <- rec$topsis$results[rec$topsis$results$State == st, ]
  pb <- place_reasons(rec, d, META, st, 3)
  w <- rec$ahp$weights[colnames(rec$X)] / sum(rec$ahp$weights)
  gap <- names(which.max(w * (1 - rec$X[st, ])))
  gap_pos <- unname(rank(-rec$X[, gap], ties.method = "min")[st])
  badge <- coverage_badge(d, META, st)
  div(class = "top-card",
      div(class = "tc-head",
          span(class = "medal", sprintf("#%d", row$Rank)),
          div(tags$h3(class = "tc-name", st), div(class = "tc-meta", STATE_TYPE[[st]]))),
      if (nzchar(badge)) div(class = "badge-low", HTML("&#9888;&#65039; "), badge),
      div(class = "match-bar", tags$i(style = sprintf("width:%.0f%%", 100 * row$Closeness))),
      div(class = "match-label", sprintf("Relative match %.2f", row$Closeness)),
      div(class = "why", tags$h4("Why it matches"),
          lapply(seq_len(nrow(pb)), function(j) why_row(pb[j, ], nrow(rec$X)))),
      div(class = "short-text", tags$b("Trade-off: "),
          sprintf("weaker on %s (%s of %d), which counts %.0f%% for you.", gap, ordinal(gap_pos), nrow(rec$X), 100 * w[[gap]])),
      tags$details(class = "card-details", tags$summary("Show the data"),
                   tags$ul(lapply(pb$Evidence, tags$li))))
}

ranking_table <- function(rec) {
  res <- rec$topsis$results
  res <- res[order(res$Rank), ]
  rows <- lapply(seq_len(nrow(res)), function(i) {
    st <- res$State[i]
    tags$tr(tags$td(res$Rank[i]), tags$td(tags$b(st)), tags$td(STATE_TYPE[[st]]),
            tags$td(tags$span(class = "bar", style = sprintf("width:%.0fpx", 90 * res$Closeness[i])),
                    sprintf("%.2f", res$Closeness[i])),
            tags$td(coverage_badge(d, META, st)))
  })
  div(class = "panel-card",
      tags$input(type = "search", class = "table-search", placeholder = "Search a State/UT...",
                 oninput = "ilmFilterTable(this)"),
      div(class = "table-scroll",
          tags$table(class = "rank-table", id = "rank-table",
                     tags$thead(tags$tr(lapply(c("Rank", "State/UT", "Type", "Relative match", "Data"),
                                               function(h) tags$th(h, title = "Click to sort", onclick = "ilmSortTable(this)")))),
                     tags$tbody(rows))),
      if (length(rec$excluded) > 0) {
        div(class = "note", "Not ranked (excluded by your choice): ", paste(rec$excluded, collapse = ", "))
      })
}

# Methodology and limits live here, not on the result screens (about_view()).
method_panel <- function() {
  av <- about_view()
  div(class = "panel-card prose",
      tags$h4("How it was calculated"),
      tags$ol(lapply(av$method, tags$li)),
      tags$h4("What each criterion measures"),
      tags$ul(lapply(seq_len(nrow(av$criteria)), function(i) {
        cr <- av$criteria[i, ]
        tags$li(tags$b(cr$Criterion), ": ", cr$Measures, " Not measured: ", cr$Not_Measured)
      })))
}

about_panel <- function() {
  av <- about_view()
  div(class = "panel-card prose",
      tags$p(av$scope),
      tags$h4("Limits to keep in mind"),
      tags$ul(lapply(av$limitations, tags$li)),
      tags$h4("Before you move, check"),
      tags$ul(lapply(av$before_you_move, tags$li)),
      tags$details(class = "how", tags$summary("How the scores were built"),
                   tags$ul(lapply(av$data_notes, tags$li))),
      tags$p(class = "note", "Personalized State and UT Livability Recommendation System. Programming for Data Science (BCSE207L)."))
}

weights_panel <- function(rec) {
  wv <- weights_view(rec$ahp)
  rows <- lapply(seq_len(nrow(wv)), function(i) {
    icon <- QUESTIONS$Icon[QUESTIONS$Criterion == wv$Criterion[i]]
    div(class = "w-row", title = CRITERION_INFO$Measures[CRITERION_INFO$Criterion == wv$Criterion[i]],
        span(HTML(icon), " ", wv$Criterion[i]),
        div(class = "w-bar", tags$i(style = sprintf("width:%.0f%%", 100 * wv$Weight[i] / max(wv$Weight)))),
        span(wv$Percent[i]))
  })
  div(class = "panel-card",
      tags$h4("What matters most to you"),
      rows,
      div(class = "note", consistency_message(rec$ahp)))
}

# ---- What if? Change several ratings, compare with the original answers ------
# preferences_view() rows -> "Show results" replaces the current answers (same
# engine, same exclusion rule); whatif_view() compares them with the original.
whatif_panel <- function(answers) {
  pv <- preferences_view(answers)
  div(class = "panel-card",
      tags$h4("What if your priorities were different?"),
      tags$p(class = "note", "Change any ratings, then show the results. Your original answers stay saved."),
      div(class = "pref-grid",
          lapply(seq_len(nrow(pv)), function(i) {
            selectInput(paste0("pref_", pv$Criterion[i]), HTML(paste(QUESTIONS$Icon[i], pv$Criterion[i])), selectize = FALSE,
                        choices = setNames(1:5, paste(1:5, ANSWER_LABELS, sep = " - ")), selected = pv$Rating[i])
          })),
      div(class = "whatif-actions",
          ui_button("pref_apply", "Show results with these priorities", "btn-soft"),
          ui_button("restore_previous", "Back to my original answers", "btn-ghost")),
      uiOutput("whatif_result"))
}

# ---- Compare: one trade-off explorer ----------------------------------------
# Two criteria on a scatter plot (tradeoff_chart_data() + render_tradeoff_plot()),
# places chosen by clicking points or searching (one input, cmp_places, holds the
# selection), a card per selected place, and all criteria on request.
compare_panel <- function(top3, axes = c("Safety", "Economy")) {
  div(class = "panel-card",
      tags$h4("Compare places"),
      tags$p(class = "note tc-intro", "Explore how States and UTs compare on two criteria. Your overall recommendation also considers your other priorities."),
      div(class = "tc-toolbar",
          selectInput("tc_x", "Horizontal axis", choices = CRITERIA, selected = axes[1], selectize = FALSE),
          selectInput("tc_y", "Vertical axis", choices = CRITERIA, selected = axes[2], selectize = FALSE),
          div(class = "tc-places",
              selectizeInput("cmp_places", "Places to compare (up to 3)", choices = sort(d$State), selected = top3, multiple = TRUE,
                             options = list(maxItems = 3, placeholder = "Search or click points"), width = "100%"),
              div(class = "tc-buttons",
                  ui_button("cmp_top3", "Use my top 3", "btn-ghost"),
                  ui_button("cmp_clear", "Clear", "btn-ghost")))),
      uiOutput("tc_chart"),
      uiOutput("tc_cards"),
      uiOutput("compare_more"))
}

# ---- Stability: one question, one visual answer -------------------------------
stability_panel <- function() {
  div(class = "panel-card",
      tags$h4("How stable is your result?"),
      div(class = "stab-control",
          selectInput("rb_level", "How sure are you of your answers?", width = "240px",
                      c("Very sure" = "low", "Fairly sure" = "medium", "Not sure" = "high"), selected = "medium", selectize = FALSE)),
      uiOutput("stability"))
}

catalogue_panel <- function() {
  div(class = "panel-card",
      tags$h4("All States and UTs"),
      textInput("cat_search", "Search by name", placeholder = "e.g. Kerala", width = "260px"),
      uiOutput("catalogue"),
      selectInput("cat_pick", "Details and original indicator values for", choices = c("Choose a State or UT" = "", sort(d$State)),
                  width = "340px", selectize = FALSE),
      uiOutput("cat_detail"))
}

news_panel <- function() {
  div(class = "panel-card",
      tags$h4("Livability news"),
      tags$p(class = "note", "Recent headlines from public RSS feeds (The Hindu, Mongabay India). They are external news and are not used in the rankings."),
      uiOutput("news"))
}

results_screen <- function(exclude_now, answers = EQUAL_ANSWERS, top3 = character(0), axes = c("Safety", "Economy")) {
  tabs <- list(nav_panel("Top 3", uiOutput("top3")),
               nav_panel("What if?", whatif_panel(answers)),
               nav_panel("Compare", compare_panel(top3, axes)),
               nav_panel("Stability", stability_panel()),
               nav_panel("Map", div(class = "panel-card", uiOutput("map_ui")), uiOutput("map_detail")),
               nav_panel("What matters to you", uiOutput("weights")),
               nav_panel("Full ranking", uiOutput("ranking")),
               nav_panel("States & UTs", catalogue_panel()),
               if (news_enabled()) nav_panel("News", news_panel()),
               nav_panel("How it works", method_panel()),
               nav_panel("About and limits", about_panel()))
  div(class = "screen",
      div(class = "result-head",
          div(tags$h2("Your livability matches"), uiOutput("mode_note")),
          div(class = "head-actions",
              downloadLink("report", "Download my report", class = "btn-soft"),
              ui_button("edit_answers", "Edit my answers", "btn-soft"),
              ui_button("restart", "Start over", "btn-ghost"))),
      div(class = "toggle-row",
          checkboxInput("exclude_low",
                        sprintf("Leave out places with limited data coverage (%s)", paste(LOW_CONF, collapse = ", ")),
                        value = exclude_now)),
      do.call(navset_pill, c(list(id = "result_tabs"), Filter(Negate(is.null), tabs))))
}

# ---- User interface ----------------------------------------------------------
ui <- page_fluid(
  title = "City Livability Optimizer",
  theme = bs_theme(version = 5, primary = "#4B3FA8",
                   base_font = font_collection("system-ui", "Segoe UI", "Roboto",
                                               "Helvetica Neue", "Arial", "sans-serif")),
  tags$head(tags$link(rel = "icon", type = "image/svg+xml", href = "img/logo.svg"),
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "features.css"),
            india_map_assets(), atlas_assets(), tags$script(src = "table.js")),
  div(class = "app-shell", uiOutput("screen"))
)

# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  empty_answers <- function() setNames(rep(NA_integer_, nrow(QUESTIONS)), CRITERIA)
  state <- reactiveValues(step = 0L, answers = empty_answers(), source = "questions",
                          exclude = FALSE, baseline = NULL)
  selected_state <- reactiveVal(NULL)
  
  # -- which screen is showing --
  output$screen <- renderUI({
    step <- state$step
    if (step == 0L) {
      welcome_screen()
    } else if (step <= nrow(QUESTIONS)) {
      question_screen(step, state$answers)
    } else {
      a <- isolate(state$answers)
      r0 <- compute_recommendation(a, X, LOW_CONF, isolate(state$exclude))
      results_screen(isolate(state$exclude), a, top_k(r0$topsis, 3)$State, default_tradeoff_axes(r0$ahp$weights))
    }
  })
  
  # -- navigation --
  go_results <- function() { selected_state(NULL); state$step <- nrow(QUESTIONS) + 1L }
  observeEvent(input$start, {
    state$answers <- empty_answers(); state$source <- "questions"; state$step <- 1L; state$baseline <- NULL
  })
  lapply(seq_along(PERSONAS), function(i) {
    observeEvent(input[[paste0("preset_", i)]], {
      state$answers <- setNames(as.integer(PERSONAS[[i]][CRITERIA]), CRITERIA)
      state$source <- paste("preset:", names(PERSONAS)[i]); state$step <- 1L; state$baseline <- NULL
    })
  })
  observeEvent(input$skip_all, {
    state$answers <- EQUAL_ANSWERS; state$source <- "equal"; state$baseline <- NULL; go_results()
  })
  # Each answer carries the question it came from: an answer sent from a screen
  # that has already been replaced (a double click) is ignored, and only whole
  # numbers 1-5 are accepted.
  observeEvent(input$answer, {
    req(state$step >= 1L, state$step <= nrow(QUESTIONS), is.list(input$answer))
    sent <- suppressWarnings(as.integer(input$answer$step))
    v <- suppressWarnings(as.integer(input$answer$value))
    req(length(sent) == 1, !is.na(sent), sent == state$step, length(v) == 1, !is.na(v), v >= 1L, v <= 5L)
    a <- state$answers; a[state$step] <- v; state$answers <- a
    if (state$step == nrow(QUESTIONS)) go_results() else state$step <- state$step + 1L
  })
  observeEvent(input$keep_next, {
    req(state$step >= 1L, state$step <= nrow(QUESTIONS), !is.na(state$answers[state$step]))
    if (state$step == nrow(QUESTIONS)) go_results() else state$step <- state$step + 1L
  })
  observeEvent(input$back, { state$step <- max(0L, state$step - 1L) })
  # Changing answers is the what-if workflow (through the questionnaire or the
  # "Change my priorities" tab): the original answers are kept as the baseline,
  # so the results can say what changed and offer to go back.
  keep_baseline <- function() if (is.null(state$baseline)) state$baseline <- state$answers
  observeEvent(input$edit_answers, {
    keep_baseline(); state$source <- "edited"; state$step <- 1L
  })
  observeEvent(input$pref_apply, {
    req(state$step > nrow(QUESTIONS))
    new <- vapply(CRITERIA, function(cn) suppressWarnings(as.integer(input[[paste0("pref_", cn)]])), integer(1))
    req(!anyNA(new), all(new >= 1L & new <= 5L))
    if (any(new != state$answers)) { keep_baseline(); state$answers <- new; state$source <- "edited" }
  })
  observeEvent(input$restore_previous, {
    req(!is.null(state$baseline), state$step > nrow(QUESTIONS))
    state$answers <- state$baseline; state$baseline <- NULL
    for (cn in CRITERIA) updateSelectInput(session, paste0("pref_", cn), selected = state$answers[[cn]])
  })
  # The what-if comparison: original answers (baseline) against the current ones.
  whatif <- reactive({
    b <- state$baseline
    if (is.null(b) || anyNA(b)) return(NULL)
    tryCatch(whatif_view(b, state$answers, X, LOW_CONF, state$exclude), error = function(e) NULL)
  })
  # The original-vs-current comparison (NULL when nothing changed).
  changes <- reactive({
    b <- state$baseline
    if (is.null(b) || anyNA(b) || !any(b != state$answers)) return(NULL)
    tryCatch(scenario_view(b, state$answers, X, LOW_CONF, state$exclude), error = function(e) NULL)
  })
  observeEvent(input$restart, {
    state$answers <- empty_answers(); state$source <- "questions"; state$step <- 0L
    state$baseline <- NULL; selected_state(NULL)
  })
  observeEvent(input$exclude_low, { state$exclude <- isTRUE(input$exclude_low) },
               ignoreInit = TRUE)
  # Only names of real States/UTs are accepted from the browser.
  observeEvent(input$map_selected, {
    req(is.character(input$map_selected), length(input$map_selected) == 1, input$map_selected %in% rownames(X))
    selected_state(input$map_selected)
  })
  
  # -- the recommendation (recalculated whenever answers or the switch change) --
  rec <- reactive({
    req(state$step > nrow(QUESTIONS))
    compute_recommendation(state$answers, X, LOW_CONF, state$exclude)
  })
  
  output$mode_note <- renderUI({
    req(state$step > nrow(QUESTIONS))
    wi <- whatif()
    if (!is.null(wi) && !wi$unchanged) {
      return(div(class = "whatif-banner", tags$b("What if: "), "you are viewing results for changed priorities. ", wi$summary, " ",
                 ui_button("restore_previous", "Back to my original answers", "btn-ghost")))
    }
    tags$p(if (state$source == "equal") {
      "Equal weights: you skipped the questions, so every factor counts the same."
    } else if (startsWith(state$source, "preset:")) {
      paste0("Based on your answers (started from the ", sub("^preset: ", "", state$source), " example).")
    } else {
      "Based on your answers."
    })
  })

  # Stability summary, shared by the Top 3 and Stability tabs.
  stab_sum <- reactive({
    rec()
    level <- if (isTRUE(input$rb_level %in% c("low", "medium", "high"))) input$rb_level else "medium"
    tryCatch(stability_summary_view(state$answers, X, d, META, LOW_CONF, state$exclude, ROBUST_LEVELS$ratings[[level]]),
             error = function(e) NULL)
  })

  output$top3 <- renderUI({
    r <- rec(); top <- top_k(r$topsis, 3)
    v <- recommendation_view(state$answers, X, d, META, LOW_CONF, state$exclude, 3)
    ss <- stab_sum()
    alt <- v$alternative_without_limited
    tagList(
      if (!is.null(ss)) div(class = "stab-line", span(class = paste("verdict", ss$label_class), ss$label), " ", ss$sentence),
      if (!is.null(alt)) div(class = "note", sprintf("Without places with limited data: %s.", paste(alt, collapse = ", "))),
      div(class = "top-grid", lapply(top$State, function(st) top_card(r, st))),
      div(class = "note", "Relative match compares places with each other for your priorities (1 = closest to the best place ranked). ",
          "It is not a livability percentage. ", SCOPE_NOTE))
  })

  output$map_ui <- renderUI({
    r <- rec(); render_india_map(r$topsis$results, X, LOW_CONF, MAP_DATA, notes = DATA_FLAGS)
  })
  
  output$map_detail <- renderUI({
    st <- selected_state(); r <- rec()
    if (is.null(st)) return(NULL)
    div(class = "panel-card map-detail",
        if (st %in% rownames(r$X)) {
          row <- r$topsis$results[r$topsis$results$State == st, ]
          tagList(tags$h4(sprintf("%s: rank %d of %d", st, row$Rank, nrow(r$X))), top_card(r, st))
        } else {
          tagList(tags$h4(st), tags$p("This State/UT is excluded from the current ranking."))
        })
  })
  
  output$report <- downloadHandler(
    filename = function() paste0("my_livability_report_", format(Sys.Date(), "%Y%m%d"), ".html"),
    content = function(file) {
      html <- build_report_html(rec(), X, LOW_CONF, state$source, STATE_TYPE,
                                shortlist = intersect(input$cmp_places, rownames(rec()$X)),
                                d = d, meta = META, changes = changes())
      writeLines(html, file, useBytes = TRUE)
    },
    contentType = "text/html")
  
  output$ranking <- renderUI(ranking_table(rec()))
  output$weights <- renderUI(weights_panel(rec()))

  # ---- Compare: trade-off explorer (tradeoff_chart_data() + render_tradeoff_plot()) ----
  # cmp_places is the single source of truth for the selection: the search box
  # edits it directly, and a click on a point (cmp_click) toggles that place.
  set_places <- function(x) updateSelectizeInput(session, "cmp_places", selected = x)
  observeEvent(input$cmp_top3, set_places(top_k(rec()$topsis, 3)$State))
  observeEvent(input$cmp_clear, set_places(character(0)))
  observeEvent(input$cmp_click, {
    req(is.character(input$cmp_click), length(input$cmp_click) == 1)
    set_places(toggle_place(input$cmp_places, input$cmp_click, d$State))
  })
  # The two axes must differ: choosing the other axis's criterion swaps them.
  last_axes <- reactiveVal(NULL)
  observeEvent(list(input$tc_x, input$tc_y), {
    x <- input$tc_x; y <- input$tc_y
    req(x %in% CRITERIA, y %in% CRITERIA)
    ax <- resolve_axes(x, y, last_axes())
    if (!identical(ax[1], x)) updateSelectInput(session, "tc_x", selected = ax[1])
    if (!identical(ax[2], y)) updateSelectInput(session, "tc_y", selected = ax[2])
    if (!identical(x, y)) last_axes(c(x, y))
  })
  tc_data <- reactive({
    x <- input$tc_x; y <- input$tc_y
    req(x %in% CRITERIA, y %in% CRITERIA)
    if (identical(x, y)) return(NULL)
    tradeoff_chart_data(rec(), x, y, d, META, as.character(input$cmp_places), LOW_CONF)
  })
  output$tc_chart <- renderUI({
    cd <- tc_data()
    if (is.null(cd)) return(div(class = "note", "Choose two different criteria."))
    render_tradeoff_plot(cd)
  })
  output$tc_cards <- renderUI({
    cd <- tc_data(); req(!is.null(cd))
    cards <- cd$cards
    tagList(
      if (nrow(cards) == 0) div(class = "note", "Click points or search above to compare up to three places.")
      else div(class = "tc-cards", lapply(seq_len(nrow(cards)), function(i) {
        x <- cards[i, ]
        div(class = "tc-card", `data-state` = x$State,
            div(class = "tc-card-head", tags$b(x$State), span(class = "tc-meta", sprintf("#%d of %d · relative match %.2f", x$Rank, cd$n_ranked, x$Match))),
            div(class = "tc-scores",
                span(sprintf("%s %.2f (%s)", cd$x, x$Score_X, ordinal(x$Pos_X))),
                span(sprintf("%s %.2f (%s)", cd$y, x$Score_Y, ordinal(x$Pos_Y)))),
            div(class = "tc-obs", x$Observation),
            if (nzchar(x$Warning)) div(class = "badge-low", HTML("&#9888;&#65039; "), x$Warning))
      })),
      if (length(cd$excluded_selected)) div(class = "note", sprintf("%s left out of the current ranking, so not shown.", paste(cd$excluded_selected, collapse = ", "))),
      tags$p(class = "note", cd$note))
  })
  # All seven criteria and the original indicator values, on request.
  output$compare_more <- renderUI({
    r <- rec()
    st <- intersect(as.character(input$cmp_places), rownames(r$X))
    if (length(st) < 2) return(NULL)
    cb <- comparison_bars_view(r, st, d, META, LOW_CONF)
    b <- cb$bars
    crits <- unique(b$Criterion)
    ind <- cb$indicators
    ind <- ind[order(match(ind$Criterion, crits)), ]
    ind_rows <- unique(ind[c("Criterion", "Indicator", "Label")])
    tags$details(class = "card-details", tags$summary("All seven criteria and the original indicator values"),
      tags$ul(class = "cmp-obs", lapply(cb$observations, tags$li)),
      lapply(crits, function(cn) {
        rows <- b[b$Criterion == cn, ]
        div(class = "cmp-crit",
            div(class = "cmp-crit-head", span(cn), span(class = "cmp-weight", sprintf("%s of your result", rows$Percent[1])),
                if (rows$Tie[1]) span(class = "verdict moderate", "about the same")),
            lapply(seq_len(nrow(rows)), function(i) {
              x <- rows[i, ]
              div(class = paste("cmp-row", if (x$Leads) "lead"),
                  span(class = "cmp-name", x$State),
                  div(class = "cmp-bar", tags$i(style = sprintf("width:%.0f%%", 100 * x$Score))),
                  span(class = "cmp-val", sprintf("%s%s", ordinal(x$Position), if (nzchar(x$Flag)) paste0(" ⚠ ", x$Flag) else "")))
            }))
      }),
      div(class = "table-scroll",
          tags$table(class = "rank-table", id = "indicator-table",
                     tags$thead(tags$tr(tags$th("Indicator"), lapply(st, tags$th))),
                     tags$tbody(lapply(seq_len(nrow(ind_rows)), function(i) {
                       ir <- ind_rows[i, ]
                       tags$tr(tags$td(sprintf("%s: %s", ir$Criterion, ir$Label)),
                               lapply(st, function(p) {
                                 x <- ind[ind$State == p & ind$Indicator == ir$Indicator, ]
                                 tags$td(paste0(x$Value_Text, if (!x$Status %in% c("observed", "missing")) paste0(" (", x$Status, ")") else "",
                                                if (x$Provenance == "source not traced") " *" else ""))
                               }))
                     })))),
      tags$p(class = "note", "* source not traced. Sources and reporting years are not recorded for any indicator."))
  })

  # ---- Stability: compact visual summary (stability_summary_view()) ----
  output$stability <- renderUI({
    ss <- stab_sum()
    if (is.null(ss)) return(div(class = "note", "The stability check could not run for these answers."))
    pl <- ss$places
    tagList(
      div(class = "stab-head", span(class = paste("verdict", ss$label_class), ss$label), span(class = "stab-big", ss$headline)),
      div(class = "match-bar stab-gauge", tags$i(style = sprintf("width:%.0f%%", 100 * ss$top1_share))),
      tags$p(ss$sentence),
      tags$h4(class = "stab-sub", "How often each place stays in your top 3"),
      lapply(seq_len(nrow(pl)), function(i) {
        x <- pl[i, ]
        div(class = "cmp-row",
            span(class = "cmp-name", sprintf("#%d %s", x$Rank, x$State)),
            div(class = "cmp-bar", tags$i(style = sprintf("width:%.0f%%", 100 * x$Top3_Share))),
            span(class = "cmp-val", sprintf("%s (first %s)", x$Top3_Label, x$First_Label)))
      }),
      tags$p(class = "note", ss$caveat),
      tags$details(class = "card-details", tags$summary("What does this mean?"), tags$ul(lapply(ss$explain, tags$li))))
  })

  # ---- What if? result: original vs current answers (whatif_view()) ----
  output$whatif_result <- renderUI({
    wi <- whatif()
    if (is.null(wi) || wi$unchanged) {
      return(div(class = "note", "You are viewing your original answers. Change one or more ratings above and press \"Show results with these priorities\"."))
    }
    item <- function(x, tag_txt, tag_class) tags$li(sprintf("#%d %s ", x$Rank, x$State), if (nzchar(tag_txt)) span(class = paste("move", tag_class), tag_txt))
    tagList(
      tags$p(class = "whatif-summary", wi$summary),
      div(class = "chips", lapply(seq_len(nrow(wi$changed)), function(i) {
        span(class = "chip", sprintf("%s %d \u2192 %d", wi$changed$Criterion[i], wi$changed$From[i], wi$changed$To[i]))
      })),
      div(class = "ba-grid",
          div(class = "ba-col", tags$h4("Original top 3"),
              tags$ol(class = "ba-list", lapply(seq_len(nrow(wi$before)), function(i) {
                x <- wi$before[i, ]; item(x, if (x$Status == "leaves") "leaves top 3" else "", "down")
              }))),
          div(class = "ba-col", tags$h4("With these priorities"),
              tags$ol(class = "ba-list", lapply(seq_len(nrow(wi$after)), function(i) {
                x <- wi$after[i, ]
                item(x, if (x$New) paste("new", x$Move) else x$Move, if (grepl("\u25B2", x$Move)) "up" else if (grepl("\u25BC", x$Move)) "down" else "same")
              })))),
      tags$ul(class = "whatif-why", lapply(wi$explanation, tags$li)))
  })

  # ---- States & UTs (R/catalogue.R) ----
  output$catalogue <- renderUI({
    cat <- catalogue_view(rec(), d, META)
    q <- trimws(if (is.null(input$cat_search)) "" else input$cat_search)
    # Case-insensitive plain-text match (grepl ignores ignore.case when fixed = TRUE).
    if (nzchar(q)) cat <- cat[grepl(tolower(q), tolower(cat$State), fixed = TRUE), ]
    if (nrow(cat) == 0) return(div(class = "note", "No State or UT matches that name."))
    div(class = "table-scroll",
        tags$table(class = "rank-table", id = "catalogue-table",
                   tags$thead(tags$tr(lapply(c("Rank", "State/UT", "Type", "Region", "Match score", "Missing indicators", "Data coverage"), tags$th))),
                   tags$tbody(lapply(seq_len(nrow(cat)), function(i) {
                     x <- cat[i, ]
                     tags$tr(tags$td(if (is.na(x$Rank)) "not ranked" else x$Rank), tags$td(tags$b(x$State)), tags$td(x$Type),
                             tags$td(x$Region), tags$td(if (is.na(x$Closeness)) "-" else sprintf("%.3f", x$Closeness)),
                             tags$td(x$Missing_Indicators), tags$td(if (x$Limited_Coverage) "limited" else if (x$Missing_Indicators > 0) "partial" else "complete"))
                   }))))
  })
  # Details use place_detail() on the current ranking, so positions, the
  # comparison sentence and similar places respect the low-confidence switch.
  output$cat_detail <- renderUI({
    st <- input$cat_pick
    req(is.character(st), length(st) == 1, st %in% rownames(X))
    pd <- place_detail(rec(), d, META, st, LOW_CONF)
    n_cmp <- if (pd$in_ranking) pd$n_ranked else pd$n_ranked + 1
    ev <- pd$indicators
    div(class = "panel-card",
        tags$h4(st),
        tags$p(if (pd$in_ranking) sprintf("Rank %d of %d for your priorities (relative match %.2f).", pd$rank, pd$n_ranked, pd$relative_match)
               else "Not in your current ranking (left out by your choice); positions below compare it with the places ranked."),
        tags$ul(lapply(seq_len(nrow(pd$criteria)), function(i) {
          cr <- pd$criteria[i, ]
          tags$li(sprintf("%s: relative score %.2f (%s of %d)%s", cr$Criterion, cr$Score, ordinal(cr$Position), n_cmp,
                          switch(cr$Support, full = "", partial = sprintf(", based on %d of %d indicators", cr$Available, cr$Total),
                                 thin = sprintf(", based on only %d of %d indicators", cr$Available, cr$Total),
                                 imputed = ", estimated: no indicator available")))
        })),
        tags$p(pd$standing),
        tags$p(class = "note", "Similar overall profile (does not use your priorities): ",
               paste(sprintf("%s (%.2f)", pd$similar$State, pd$similar$Distance), collapse = ", ")),
        if (sum(is.na(ev$Raw)) == 0) tags$p("All 17 indicators are available.")
        else tags$p(sprintf("%d of 17 indicators are missing.", sum(is.na(ev$Raw)))),
        if (nzchar(pd$coverage_note)) tags$p(class = "note", pd$coverage_note),
        tags$h4("Indicator values (original data)"),
        tags$ul(lapply(seq_len(nrow(ev)), function(j) tags$li(sprintf("%s (%s)", ev$Evidence[j], ev$Criterion[j])))),
        tags$p(class = "note", "Sources and reporting years are not recorded for any indicator (see docs/DATA_AUDIT.md)."))
  })
  # ---- News (R/news.R): fresh cache, else one background refresh; never blocks ----
  news <- reactiveVal(list(status = if (news_enabled()) "loading" else "off"))
  if (news_enabled()) {
    cached <- read_news_cache()
    if (!is.null(cached) && nrow(cached$items) > 0) {
      news(list(status = "ok", items = cached$items))
    } else {
      ensure_news_refresh(NEWS_STATE, ROOT)
      started <- Sys.time()
      poll <- observe({
        invalidateLater(1000)
        p <- NEWS_STATE$proc
        if (is.null(p) || !p$is_alive() || difftime(Sys.time(), started, units = "secs") > 25) {
          fresh <- read_news_cache()
          got <- if (is.null(fresh) || nrow(fresh$items) == 0) read_news_cache(max_age_hours = 48) else fresh
          news(if (!is.null(got) && nrow(got$items) > 0) list(status = "ok", items = got$items) else list(status = "unavailable"))
          poll$destroy()
        }
      })
    }
  }
  output$news <- renderUI({
    n <- news()
    switch(n$status,
           off = tags$p("News is off by default: the headlines are not about particular States/UTs and are not used in the rankings. Set LIVABILITY_NEWS=on (with the xml2 and curl packages installed) to show them."),
           loading = tags$p("Loading headlines..."),
           unavailable = tags$p("Headlines are unavailable right now. The rest of the app works as usual."),
           ok = tags$ul(id = "news-list", lapply(seq_len(nrow(n$items)), function(i) {
             it <- n$items[i, ]
             tags$li(tags$a(href = it$Link, target = "_blank", rel = "noopener noreferrer", it$Title),
                     tags$span(class = "note", sprintf(" - %s%s, %s", it$Source,
                                                       if (nzchar(news_date_label(it$Date))) paste0(", ", news_date_label(it$Date)) else "",
                                                       it$Topic)))
           })))
  })
}

shinyApp(ui, server)