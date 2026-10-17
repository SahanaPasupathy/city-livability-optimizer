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
            "report", "sensitivity", "pareto", "atlas", "catalogue", "news", "robustness", "scenario")) {
  source(file.path(ROOT, "R", paste0(f, ".R")))
}
d          <- load_livability_data(file.path(ROOT, "data", "State_UT_Livability_Dataset_Final.csv"))
X          <- criteria_matrix(d)
LOW_CONF   <- d$State[d$Low_Confidence == 1]
STATE_TYPE <- setNames(d$Type, d$State)
MAP_DATA   <- load_map_paths(file.path(ROOT, "data", "map", "india_map_paths.csv"))
META       <- load_indicator_metadata(file.path(ROOT, "data", "indicator_metadata.csv"))
# Data notes per State/UT (missing indicators, low confidence), shown with the
# robustness results so places scored on thin data are visible.
DATA_FLAGS <- vapply(d$State, function(st) {
  nt <- state_data_notes(d, META, st)
  paste(c(if (length(nt$criteria_without_data)) paste("no data for", paste(nt$criteria_without_data, collapse = ", ")),
          if (nt$low_confidence) sprintf("low confidence (%d of 17 indicators missing)", nrow(nt$missing))),
        collapse = "; ")
}, character(1))
DATA_FLAGS <- DATA_FLAGS[nzchar(DATA_FLAGS)]
# Robustness settings offered in the app. "How sure are you?" maps to the chance
# that each rating is one point off (ratings method) or to the Dirichlet
# concentration (weights method). These are assumptions the user chooses, not
# estimates of how people answer.
ROBUST_LEVELS <- list(ratings = c(low = 0.10, medium = 0.25, high = 0.50),
                      dirichlet = c(low = 200, medium = 50, high = 15))
ROBUST_N <- c(500, 1000, 2000, 5000)
# News headlines are refreshed by one background process shared by all sessions.
NEWS_STATE <- new.env()

PRESET_BLURB <- c("Economy, connectivity and housing first",
                  "Education, safety and health first",
                  "Health, environment and safety first")
VERDICT_CLASS <- c("Strong match" = "strong", "Moderate match" = "moderate",
                   "Falls short" = "short")
VERDICT_COLOUR <- c("Strong match" = "#10B981", "Moderate match" = "#F59E0B",
                    "Falls short" = "#EF4444")

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
                 "Find the State or UT that fits your life best. Answer 7 quick questions and get your personal top 3 out of 36 States and UTs."),
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
                   tags$p("You tell us how important seven things are to you (education, safety, health, housing, economy, environment, connectivity). ",
                          "AHP turns your answers into weights and checks that they are consistent. ",
                          "TOPSIS then ranks every State and UT by how close it is to the best possible score on the things YOU care about, ",
                          "and how far it is from the worst.")))
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
          tags$h2(class = "fc-title", "How important is ", tags$span(q$Criterion), " to you?"),
          tags$p(class = "fc-hint", q$Hint),
          div(class = "scale", buttons)),
      div(class = "fc-nav",
          ui_button("back", HTML("&larr; Back"), "btn-ghost"),
          if (!is.na(current)) ui_button("keep_next", HTML("Keep this answer &rarr;"), "btn-soft")))
}

why_row <- function(row) {
  vclass <- VERDICT_CLASS[[row$Verdict]]
  icon <- QUESTIONS$Icon[QUESTIONS$Criterion == row$Criterion]
  div(class = "why-row",
      div(class = "why-top", span(HTML(icon), " ", row$Criterion),
          span(class = paste("verdict", vclass), row$Verdict)),
      div(class = "why-bar", tags$i(style = sprintf("width:%.0f%%;background:%s", 100 * row$Score,
                                                    VERDICT_COLOUR[[row$Verdict]]))),
      div(class = "why-text", row$Sentence))
}

top_card <- function(rec, row, i) {
  st  <- row$State
  pb  <- priority_breakdown(rec$topsis, rec$ahp$weights, st, 3)
  sim <- similar_states(rec$X, st, 3)
  div(class = paste("top-card", c("gold", "silver", "bronze")[i]),
      div(class = "tc-head",
          span(class = "medal", HTML(c("&#129351;", "&#129352;", "&#129353;")[i])),
          div(tags$h3(class = "tc-name", st),
              div(class = "tc-meta", sprintf("%s | rank %d of %d", STATE_TYPE[[st]], row$Rank, nrow(rec$X))))),
      if (st %in% LOW_CONF) div(class = "badge-low", HTML("&#9888;&#65039; Low data confidence")),
      div(class = "match-bar", tags$i(style = sprintf("width:%.0f%%", 100 * row$Closeness))),
      div(class = "match-label", sprintf("Match score (TOPSIS closeness): %.3f", row$Closeness)),
      div(class = "why", tags$h4("Why this fits you"),
          lapply(seq_len(nrow(pb)), function(j) why_row(pb[j, ])),
          div(class = "short-text", shortfall_sentence(rec$topsis, rec$ahp$weights, st))),
      div(class = "similar", "Similar States/UTs (same profile, ignores your weights):",
          div(class = "chips", lapply(seq_len(nrow(sim)), function(j) {
            span(class = "chip", sprintf("%s (%.2f)", sim$State[j], sim$Distance[j]))
          }))))
}

ranking_table <- function(rec) {
  res <- rec$topsis$results
  res <- res[order(res$Rank), ]
  rows <- lapply(seq_len(nrow(res)), function(i) {
    st <- res$State[i]
    tags$tr(tags$td(res$Rank[i]), tags$td(tags$b(st)), tags$td(STATE_TYPE[[st]]),
            tags$td(tags$span(class = "bar", style = sprintf("width:%.0fpx", 90 * res$Closeness[i])),
                    sprintf("%.3f", res$Closeness[i])),
            tags$td(sprintf("%.3f", res$D_plus[i])), tags$td(sprintf("%.3f", res$D_minus[i])),
            tags$td(if (st %in% LOW_CONF) "low confidence" else ""))
  })
  div(class = "panel-card",
      tags$input(type = "search", class = "table-search", placeholder = "Search a State/UT...",
                 oninput = "ilmFilterTable(this)"),
      div(class = "table-scroll",
          tags$table(class = "rank-table", id = "rank-table",
                     tags$thead(tags$tr(lapply(c("Rank", "State/UT", "Type", "Match (closeness)",
                                                 "Distance to ideal (D+)", "Distance to worst (D-)", "Note"),
                                               function(h) tags$th(h, title = "Click to sort", onclick = "ilmSortTable(this)")))),
                     tags$tbody(rows))),
      if (length(rec$excluded) > 0) {
        div(class = "note", "Not ranked (excluded by your choice): ", paste(rec$excluded, collapse = ", "))
      })
}

method_panel <- function() {
  div(class = "panel-card prose",
      tags$h4("How it was calculated"),
      tags$ol(lapply(METHOD_STEPS, tags$li)),
      tags$h4("What each criterion is built from"),
      tags$ul(lapply(CRITERIA, function(cn) tags$li(tags$b(cn), ": ", CRITERION_INDICATORS[[cn]]))))
}

about_panel <- function() {
  div(class = "panel-card prose",
      tags$h4("About this project"),
      tags$p("Personalized State and UT Livability Recommendation System. Programming for Data Science (BCSE207L), ",
             "built in R with AHP, TOPSIS and Shiny."),
      tags$h4("Limits to keep in mind"),
      tags$ul(lapply(LIMITATIONS, tags$li)))
}

weights_panel <- function(rec) {
  wt <- weights_table(rec$ahp)
  rows <- lapply(seq_len(nrow(wt)), function(i) {
    icon <- QUESTIONS$Icon[QUESTIONS$Criterion == wt$Criterion[i]]
    div(class = "w-row", span(HTML(icon), " ", wt$Criterion[i]),
        div(class = "w-bar", tags$i(style = sprintf("width:%.0f%%", 100 * wt$Weight[i] / max(wt$Weight)))),
        span(sprintf("%.1f%%", 100 * wt$Weight[i])))
  })
  div(class = "panel-card",
      tags$h4("How much each factor counts for you"),
      rows,
      div(class = paste("cr-box", if (rec$ahp$consistent) "ok" else "warn"),
          HTML(if (rec$ahp$consistent) "&#9989; " else "&#9888;&#65039; "),
          consistency_message(rec$ahp)),
      div(class = "note", "Weights come from AHP: your 1-5 answers become pairwise comparisons, ",
          "and the weights are the principal eigenvector of that comparison matrix."))
}

# ---- Panels for the added features (plain layout, original styles) ---------------
atlas_panel <- function() {
  div(class = "panel-card",
      tags$h4("Trade-offs between two criteria"),
      tags$p(class = "note", "Choose two criteria. Filled points are Pareto-efficient: no other State/UT is at least as good on both and better on one. ",
             "There is usually no single best place, only different trade-offs; this view ignores your weights, while your ranking uses them."),
      div(class = "toggle-row",
          selectInput("atlas_x", "Horizontal axis", choices = CRITERIA, selected = "Safety", width = "220px", selectize = FALSE),
          selectInput("atlas_y", "Vertical axis", choices = CRITERIA, selected = "Economy", width = "220px", selectize = FALSE)),
      uiOutput("atlas"))
}

robustness_panel <- function() {
  div(class = "panel-card",
      tags$h4("How robust is your ranking?"),
      tags$p(class = "note", "Your ranking is re-run many times with slightly different preferences. The shares say how often each place stays near the top under these assumptions; ",
             "they are not probabilities that a place is the best place to live."),
      div(class = "toggle-row",
          selectInput("rb_method", "What may change", width = "300px",
                      c("My ratings (each could be one point off)" = "ratings", "My weights (small random changes)" = "dirichlet"), selectize = FALSE),
          selectInput("rb_level", "How sure are you of them?", width = "200px",
                      c("Very sure" = "low", "Fairly sure" = "medium", "Not sure" = "high"), selected = "medium", selectize = FALSE),
          selectInput("rb_n", "Simulations", width = "140px", setNames(ROBUST_N, format(ROBUST_N, big.mark = ",")), selected = 1000, selectize = FALSE),
          numericInput("rb_seed", "Random seed", value = 2026, min = 1, max = 999999, step = 1, width = "140px"),
          actionButton("rb_run", "Run simulation")),
      uiOutput("robustness"),
      downloadLink("rb_csv", "Download all 36 results (CSV)"))
}

whatif_panel <- function() {
  div(class = "panel-card",
      tags$h4("What if one of your ratings changed?"),
      tags$p(class = "note", "Pick a criterion and a different rating. The scenario is ranked with the same method and the same low-confidence setting; your results above do not change."),
      div(class = "toggle-row",
          selectInput("wi_crit", "Criterion", choices = CRITERIA, width = "220px", selectize = FALSE),
          selectInput("wi_value", "New rating", choices = setNames(1:5, paste(1:5, ANSWER_LABELS, sep = " - ")), width = "220px", selectize = FALSE)),
      uiOutput("whatif"))
}

catalogue_panel <- function() {
  div(class = "panel-card",
      tags$h4("All States and UTs"),
      tags$p(class = "note", "Ranks and match scores use your current weights. Data notes come from the dataset."),
      textInput("cat_search", "Search by name", placeholder = "e.g. Kerala", width = "260px"),
      uiOutput("catalogue"),
      selectInput("cat_pick", "Details for", choices = c("Choose a State or UT" = "", sort(d$State)), width = "300px", selectize = FALSE),
      uiOutput("cat_detail"),
      tags$h4("Compare up to three"),
      selectizeInput("cmp_pick", NULL, choices = sort(d$State), multiple = TRUE,
                     options = list(maxItems = 3, placeholder = "Choose States/UTs"), width = "100%"),
      uiOutput("compare"),
      tags$p(class = "note", "The comparison and any what-if scenario are included in your downloaded report."))
}

news_panel <- function() {
  div(class = "panel-card",
      tags$h4("Livability news"),
      tags$p(class = "note", "Recent headlines from public RSS feeds (The Hindu, Mongabay India). They are external news and are not used in the rankings."),
      uiOutput("news"))
}

results_screen <- function(exclude_now) {
  div(class = "screen",
      div(class = "result-head",
          div(tags$h2("Your livability matches"), uiOutput("mode_note")),
          div(class = "head-actions",
              downloadLink("report", "Download my report", class = "btn-soft"),
              ui_button("edit_answers", "Edit my answers", "btn-soft"),
              ui_button("restart", "Start over", "btn-ghost"))),
      div(class = "toggle-row",
          checkboxInput("exclude_low",
                        sprintf("Leave out low-confidence States/UTs (%s)", paste(LOW_CONF, collapse = ", ")),
                        value = exclude_now)),
      navset_pill(id = "result_tabs",
                  nav_panel("Top 3", uiOutput("top3")),
                  nav_panel("Map", div(class = "panel-card", uiOutput("map_ui")), uiOutput("map_detail")),
                  nav_panel("Full ranking", uiOutput("ranking")),
                  nav_panel("My weights", uiOutput("weights")),
                  nav_panel("Trade-offs", atlas_panel()),
                  nav_panel("Robustness", robustness_panel()),
                  nav_panel("What if", whatif_panel()),
                  nav_panel("States & UTs", catalogue_panel()),
                  nav_panel("News", news_panel()),
                  nav_panel("How it works", method_panel()),
                  nav_panel("About and limits", about_panel())))
}

# ---- User interface ----------------------------------------------------------
ui <- page_fluid(
  title = "City Livability Optimizer",
  theme = bs_theme(version = 5, primary = "#4B3FA8",
                   base_font = font_collection("system-ui", "Segoe UI", "Roboto",
                                               "Helvetica Neue", "Arial", "sans-serif")),
  tags$head(tags$link(rel = "icon", type = "image/svg+xml", href = "img/logo.svg"),
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
            india_map_assets(), atlas_assets(), tags$script(src = "table.js")),
  div(class = "app-shell", uiOutput("screen"))
)

# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  empty_answers <- function() setNames(rep(NA_integer_, nrow(QUESTIONS)), CRITERIA)
  state <- reactiveValues(step = 0L, answers = empty_answers(), source = "questions",
                          exclude = FALSE)
  selected_state <- reactiveVal(NULL)
  
  # -- which screen is showing --
  output$screen <- renderUI({
    step <- state$step
    if (step == 0L) {
      welcome_screen()
    } else if (step <= nrow(QUESTIONS)) {
      question_screen(step, state$answers)
    } else {
      results_screen(isolate(state$exclude))
    }
  })
  
  # -- navigation --
  go_results <- function() { selected_state(NULL); state$step <- nrow(QUESTIONS) + 1L }
  observeEvent(input$start, {
    state$answers <- empty_answers(); state$source <- "questions"; state$step <- 1L
  })
  lapply(seq_along(PERSONAS), function(i) {
    observeEvent(input[[paste0("preset_", i)]], {
      state$answers <- setNames(as.integer(PERSONAS[[i]][CRITERIA]), CRITERIA)
      state$source <- paste("preset:", names(PERSONAS)[i]); state$step <- 1L
    })
  })
  observeEvent(input$skip_all, {
    state$answers <- EQUAL_ANSWERS; state$source <- "equal"; go_results()
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
  observeEvent(input$edit_answers, { state$source <- "edited"; state$step <- 1L })
  observeEvent(input$restart, {
    state$answers <- empty_answers(); state$source <- "questions"; state$step <- 0L
    selected_state(NULL)
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
    tags$p(if (state$source == "equal") {
      "Equal weights: you skipped the questions, so every factor counts the same."
    } else if (startsWith(state$source, "preset:")) {
      paste0("Based on your answers (started from the ", sub("^preset: ", "", state$source), " preset).")
    } else {
      "Based on your answers."
    })
  })
  
  output$top3 <- renderUI({
    r <- rec(); top <- top_k(r$topsis, 3)
    tagList(div(class = "top-grid", lapply(seq_len(nrow(top)), function(i) top_card(r, top[i, ], i))),
            div(class = "note", "Closeness runs from 0 (worst possible) to 1 (the ideal State/UT for you). ",
                "Similar States/UTs are found by comparing all seven scores equally, so they are not the same as the next-ranked ones."))
  })
  
  output$map_ui <- renderUI({
    r <- rec(); render_india_map(r$topsis$results, X, LOW_CONF, MAP_DATA)
  })
  
  output$map_detail <- renderUI({
    st <- selected_state(); r <- rec()
    if (is.null(st)) return(NULL)
    div(class = "panel-card map-detail",
        if (st %in% rownames(r$X)) {
          row <- r$topsis$results[r$topsis$results$State == st, ]
          tagList(tags$h4(sprintf("%s: rank %d of %d", st, row$Rank, nrow(r$X))),
                  tags$ul(lapply(explain_state(r$topsis, r$ahp$weights, st, LOW_CONF), tags$li)))
        } else {
          tagList(tags$h4(st), tags$p("This State/UT is excluded from the current ranking."))
        })
  })
  
  output$report <- downloadHandler(
    filename = function() paste0("my_livability_report_", format(Sys.Date(), "%Y%m%d"), ".html"),
    content = function(file) {
      sc <- tryCatch(scenario(), error = function(e) NULL)
      html <- build_report_html(rec(), X, LOW_CONF, state$source, STATE_TYPE,
                                shortlist = intersect(input$cmp_pick, rownames(rec()$X)),
                                scenario = if (!is.null(sc) && length(sc$changed) > 0) sc$scenario else NULL,
                                pareto = pareto_front(rec()$X))
      writeLines(html, file, useBytes = TRUE)
    },
    contentType = "text/html")
  
  output$ranking <- renderUI(ranking_table(rec()))
  output$weights <- renderUI(weights_panel(rec()))

  # ---- Trade-offs (Pareto efficiency on two criteria) ----
  output$atlas <- renderUI({
    r <- rec()
    x <- input$atlas_x; y <- input$atlas_y
    req(x %in% CRITERIA, y %in% CRITERIA)
    if (x == y) return(div(class = "note", "Choose two different criteria."))
    front <- pareto_front(r$X, c(x, y))
    top1 <- top_k(r$topsis, 1)$State
    f7 <- pareto_front(r$X)
    tagList(render_atlas(r$X, r$topsis$results, x, y, LOW_CONF, selected = isolate(selected_state())),
            tags$p(sprintf("Your #1 match, %s: %s", top1, explain_pareto(front, top1))),
            tags$p(class = "note", sprintf("Across all seven criteria at once, %d of %d places are Pareto-efficient, which is why two criteria at a time are more informative.",
                                           sum(f7$Efficient, na.rm = TRUE), nrow(r$X))))
  })

  # ---- Robustness (R/robustness.R) ----
  robust <- eventReactive(list(input$rb_run, rec()), {
    r <- rec()
    method <- if (identical(input$rb_method, "dirichlet")) "dirichlet" else "ratings"
    level <- if (isTRUE(input$rb_level %in% c("low", "medium", "high"))) input$rb_level else "medium"
    n <- suppressWarnings(as.integer(input$rb_n)); if (length(n) != 1 || is.na(n) || !n %in% ROBUST_N) n <- 1000L
    seed <- suppressWarnings(as.numeric(input$rb_seed)); if (length(seed) != 1 || !is.finite(seed) || seed < 1) seed <- 2026
    tryCatch(robustness_analysis(r$X, answers = state$answers, weights = r$ahp$weights, preference = method,
                                 n = n, seed = round(seed), p_change = ROBUST_LEVELS$ratings[[level]],
                                 concentration = ROBUST_LEVELS$dirichlet[[level]], flags = DATA_FLAGS),
             error = function(e) structure(list(message = conditionMessage(e)), class = "robust_error"))
  }, ignoreNULL = FALSE)
  output$robustness <- renderUI({
    rb <- robust()
    if (inherits(rb, "robust_error")) return(div(class = "note", "The simulation could not run: ", rb$message))
    pct <- function(x) sprintf("%.0f%%", 100 * x)
    n_txt <- format(rb$settings$n, big.mark = ",")
    s <- rb$summary
    base_top3 <- rb$baseline$results$State[rb$baseline$results$Rank <= 3]
    show <- rbind(head(s, 10), s[s$State %in% setdiff(base_top3, head(s, 10)$State), ])
    rows <- lapply(seq_len(nrow(show)), function(i) {
      x <- show[i, ]
      tags$tr(tags$td(tags$b(x$State)), tags$td(x$Base_Rank), tags$td(sprintf("%.3f", x$Base_Closeness)),
              tags$td(pct(x$Top1_Prop)), tags$td(pct(x$Top3_Prop)), tags$td(pct(x$Top5_Prop)), tags$td(pct(x$Top10_Prop)),
              tags$td(sprintf("%d (%d-%d)", as.integer(x$Rank_Median), as.integer(x$Rank_Q05), as.integer(x$Rank_Q95))),
              tags$td(x$Data_Note))
    })
    infl <- rb$influence
    strong <- if (!is.null(infl)) infl[!is.na(infl$Rho_Top_Rank) & abs(infl$Rho_Top_Rank) >= 0.2, ] else NULL
    assumption <- if (rb$settings$preference == "ratings") {
      sprintf("each of your seven ratings had a %s chance of being one point higher or lower (half each way, kept within 1-5), and the weights were recomputed with AHP",
              pct(rb$settings$p_change))
    } else {
      w <- max(rb$baseline$weights)
      sprintf("your weights were drawn from a Dirichlet distribution centred on your current weights (concentration %s; a weight of %s typically moves by about %.1f percentage points)",
              rb$settings$concentration, pct(w), 100 * dirichlet_weight_sd(w, rb$settings$concentration))
    }
    tagList(
      tags$p(HTML(sprintf("Your #1, <b>%s</b>, stays first in %s and in your top 3 in %s of %s simulations.",
                          htmltools::htmlEscape(rb$baseline$state), pct(rb$baseline$top1_prop), pct(rb$baseline$top3_prop), n_txt))),
      if (nrow(rb$competitive) > 0) {
        tags$p(paste0("Also often in your top 3: ", paste(sprintf("%s (%s)", rb$competitive$State, pct(rb$competitive$Top3_Prop)), collapse = ", "), "."))
      },
      div(class = "table-scroll",
          tags$table(class = "rank-table", id = "robust-table",
                     tags$thead(tags$tr(lapply(c("State/UT", "Your rank", "Match score", "Rank 1", "Top 3", "Top 5", "Top 10",
                                                 "Simulated rank: median (5-95%)", "Data note"), tags$th))),
                     tags$tbody(rows))),
      tags$p(class = "note", "\"Your rank\" and \"Match score\" are your actual TOPSIS results. The other columns are simulated."),
      if (!is.null(strong) && nrow(strong) > 0) {
        tags$p(paste0("What moves ", rb$baseline$state, " most: ",
                      paste(sprintf("%s (more weight tends to move it %s)", strong$Criterion, ifelse(strong$Rho_Top_Rank > 0, "down", "up")),
                            collapse = "; "),
                      ". This is a rank correlation across the simulations, not a cause."))
      },
      tags$ul(class = "note",
              tags$li(sprintf("Assumption: in each simulation %s. Random seed %s gives the same numbers every time.", assumption,
                              format(rb$settings$seed, scientific = FALSE))),
              tags$li("Data uncertainty is not modelled: the dataset records no sources, years or error ranges for its indicators (see docs/DATA_AUDIT.md). Places with missing indicators are flagged above; their scores are used as stored."),
              tags$li(if (length(rb$not_comparable)) paste("Not compared (missing score):", paste(rb$not_comparable, collapse = ", "))
                      else "Every ranked State/UT has all seven criterion scores, so none was left out.")))
  })
  output$rb_csv <- downloadHandler(
    filename = function() "livability_robustness.csv",
    content = function(file) {
      rb <- robust(); req(!inherits(rb, "robust_error"))
      utils::write.csv(rb$summary, file, row.names = FALSE)
    })

  # ---- What if (R/scenario.R) ----
  observeEvent(list(input$wi_crit, rec()), {
    cn <- input$wi_crit; req(cn %in% CRITERIA)
    updateSelectInput(session, "wi_value", selected = state$answers[[cn]])
  })
  scenario <- reactive({
    rec()
    cn <- input$wi_crit; v <- suppressWarnings(as.integer(input$wi_value))
    req(cn %in% CRITERIA, length(v) == 1, !is.na(v))
    run_scenario(state$answers, setNames(v, cn), X, LOW_CONF, state$exclude)
  })
  output$whatif <- renderUI({
    sc <- tryCatch(scenario(), error = function(e) e)
    if (inherits(sc, "error")) return(div(class = "note", "This scenario cannot be calculated: ", conditionMessage(sc)))
    cn <- input$wi_crit
    bw <- sc$base$ahp$weights[[cn]]; sw <- sc$scenario$ahp$weights[[cn]]
    labels <- tags$p(sprintf("Baseline: %s rated %d (weight %.1f%%). Scenario: %s rated %d (weight %.1f%%).",
                             cn, sc$answers[[cn]], 100 * bw, cn, sc$scenario_answers[[cn]], 100 * sw))
    if (length(sc$changed) == 0) {
      return(tagList(labels, tags$p(class = "note", "Choose a rating different from your current one to see a scenario.")))
    }
    top <- head(sc$changes[order(sc$changes$Personal_Rank), ], 10)
    tagList(labels,
            tags$p(if (sc$new_top) HTML(sprintf("Your #1 would change from <b>%s</b> to <b>%s</b>.", htmltools::htmlEscape(sc$base_top), htmltools::htmlEscape(sc$scenario_top)))
                   else sprintf("Your #1 would stay %s.", sc$base_top)),
            div(class = "table-scroll",
                tags$table(class = "rank-table", id = "whatif-table",
                           tags$thead(tags$tr(lapply(c("Scenario rank", "State/UT", "Baseline rank", "Change"), tags$th))),
                           tags$tbody(lapply(seq_len(nrow(top)), function(i) {
                             k <- top$Rank_Change[i]
                             tags$tr(tags$td(top$Personal_Rank[i]), tags$td(top$State[i]), tags$td(top$Baseline_Rank[i]),
                                     tags$td(if (k > 0) sprintf("up %d", k) else if (k < 0) sprintf("down %d", -k) else "no change"))
                           })))))
  })

  # ---- States & UTs (R/catalogue.R) ----
  output$catalogue <- renderUI({
    cat <- catalogue_data(rec(), d)
    q <- trimws(if (is.null(input$cat_search)) "" else input$cat_search)
    # Case-insensitive plain-text match (grepl ignores ignore.case when fixed = TRUE).
    if (nzchar(q)) cat <- cat[grepl(tolower(q), tolower(cat$State), fixed = TRUE), ]
    if (nrow(cat) == 0) return(div(class = "note", "No State or UT matches that name."))
    div(class = "table-scroll",
        tags$table(class = "rank-table", id = "catalogue-table",
                   tags$thead(tags$tr(lapply(c("Rank", "State/UT", "Type", "Region", "Match score", "Missing indicators", "Data confidence"), tags$th))),
                   tags$tbody(lapply(seq_len(nrow(cat)), function(i) {
                     x <- cat[i, ]
                     tags$tr(tags$td(if (is.na(x$Rank)) "not ranked" else x$Rank), tags$td(tags$b(x$State)), tags$td(x$Type),
                             tags$td(x$Region), tags$td(if (is.na(x$Closeness)) "-" else sprintf("%.3f", x$Closeness)),
                             tags$td(x$Missing_Indicators), tags$td(if (x$Low_Confidence) "low" else "normal"))
                   }))))
  })
  output$cat_detail <- renderUI({
    st <- input$cat_pick
    req(is.character(st), length(st) == 1, st %in% rownames(X))
    nt <- state_data_notes(d, META, st)
    pos <- vapply(CRITERIA, function(cn) unname(rank(-X[, cn], ties.method = "min")[st]), numeric(1))
    div(class = "panel-card",
        tags$h4(st),
        tags$ul(lapply(CRITERIA, function(cn) tags$li(sprintf("%s: %.2f (%s of 36)", cn, X[st, cn], ordinal(pos[[cn]]))))),
        tags$p(explain_pareto(pareto_front(X), st)),
        if (nrow(nt$missing) == 0) tags$p("All 17 indicators are available.")
        else tagList(
          tags$p(sprintf("%d of 17 indicators are missing:", nrow(nt$missing))),
          tags$ul(lapply(seq_len(nrow(nt$missing)), function(j) tags$li(sprintf("%s (%s)", nt$missing$Label[j], nt$missing$Criterion[j])))),
          if (length(nt$criteria_without_data)) tags$p(sprintf("No indicator is available for %s, so that score is not based on measured data (see docs/DATA_AUDIT.md).",
                                                               paste(nt$criteria_without_data, collapse = ", "))),
          if (length(nt$partial)) tags$p(sprintf("Scored on the available indicators only: %s.", paste(nt$partial, collapse = ", ")))),
        if (nt$low_confidence) tags$p(class = "note", "Flagged low data confidence."))
  })
  output$compare <- renderUI({
    r <- rec()
    states <- intersect(input$cmp_pick, rownames(r$X))
    left_out <- setdiff(input$cmp_pick, states)
    if (length(states) == 0) return(div(class = "note", "Choose one to three States or UTs."))
    res <- r$topsis$results
    wt <- weights_table(r$ahp)
    tagList(
      div(class = "table-scroll",
          tags$table(class = "rank-table", id = "compare-table",
                     tags$thead(tags$tr(tags$th("Criterion (your weight)"), lapply(states, tags$th))),
                     tags$tbody(lapply(seq_len(nrow(wt)), function(i) {
                       cn <- wt$Criterion[i]
                       tags$tr(tags$td(sprintf("%s (%.1f%%)", cn, 100 * wt$Weight[i])),
                               lapply(states, function(st) tags$td(sprintf("%.2f", r$X[st, cn]))))
                     }),
                     tags$tr(tags$td(tags$b("Match score (rank)")),
                             lapply(states, function(st) {
                               x <- res[res$State == st, ]
                               tags$td(tags$b(sprintf("%.3f (%d)", x$Closeness, x$Rank)))
                             }))))),
      if (length(left_out)) div(class = "note", paste0(paste(left_out, collapse = ", "), " left out: excluded from the current ranking.")))
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
           off = tags$p("News is switched off (LIVABILITY_NEWS=off), or the xml2 and curl packages are not installed."),
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