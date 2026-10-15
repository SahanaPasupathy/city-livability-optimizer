# ==============================================================================
# City Livability Optimizer - Shiny app (Step 7a)
# ------------------------------------------------------------------------------
# Flow : welcome -> seven flashcard questions (1-5) -> results.
# Calculations are NOT done here: they come from the tested functions in R/
# (AHP weights, TOPSIS ranking, explanations, similar States, map). This file
# only decides which screen to show and reacts to clicks.
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
for (f in c("common", "ahp", "topsis", "similarity", "explain", "map", "app_logic")) {
  source(file.path(ROOT, "R", paste0(f, ".R")))
}
d          <- load_livability_data(file.path(ROOT, "data", "State_UT_Livability_Dataset_Final.csv"))
X          <- criteria_matrix(d)
LOW_CONF   <- d$State[d$Low_Confidence == 1]
STATE_TYPE <- setNames(d$Type, d$State)
MAP_DATA   <- load_map_paths(file.path(ROOT, "data", "map", "india_map_paths.csv"))

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
          div(class = "logo", HTML("&#127961;&#65039;")),
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
                onclick = sprintf("Shiny.setInputValue('answer', %d, {priority: 'event'})", v),
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
      div(class = "table-scroll",
          tags$table(class = "rank-table",
                     tags$thead(tags$tr(lapply(c("Rank", "State/UT", "Type", "Match (closeness)",
                                                 "Distance to ideal (D+)", "Distance to worst (D-)", "Note"),
                                               tags$th))),
                     tags$tbody(rows))),
      if (length(rec$excluded) > 0) {
        div(class = "note", "Not ranked (excluded by your choice): ", paste(rec$excluded, collapse = ", "))
      })
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

results_screen <- function(exclude_now) {
  div(class = "screen",
      div(class = "result-head",
          div(tags$h2("Your livability matches"), uiOutput("mode_note")),
          div(class = "head-actions",
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
                  nav_panel("My weights", uiOutput("weights"))))
}

# ---- User interface ----------------------------------------------------------
ui <- page_fluid(
  title = "City Livability Optimizer",
  theme = bs_theme(version = 5, primary = "#4B3FA8",
                   base_font = font_collection("system-ui", "Segoe UI", "Roboto",
                                               "Helvetica Neue", "Arial", "sans-serif")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
            india_map_assets()),
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
  observeEvent(input$answer, {
    req(state$step >= 1L, state$step <= nrow(QUESTIONS))
    a <- state$answers; a[state$step] <- as.integer(input$answer); state$answers <- a
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
  observeEvent(input$map_selected, { selected_state(input$map_selected) })
  
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
  
  output$ranking <- renderUI(ranking_table(rec()))
  output$weights <- renderUI(weights_panel(rec()))
}

shinyApp(ui, server)