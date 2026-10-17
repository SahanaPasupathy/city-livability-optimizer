# ==============================================================================
# Personal livability report: one self-contained HTML file the user can save,
# print or send. All numbers come from the same functions the app uses.
# ==============================================================================

REPORT_CSS <- "
body{font-family:system-ui,'Segoe UI',Roboto,Arial,sans-serif;color:#1f2937;max-width:860px;margin:0 auto;padding:28px 20px;background:#fff;line-height:1.5}
h1{color:#4B3FA8;margin:0 0 4px}h2{color:#4B3FA8;border-bottom:2px solid #E8E6FA;padding-bottom:4px;margin-top:34px}
.sub{color:#6b7280;margin:0 0 18px}.card{border:1px solid #E5E7EB;border-radius:14px;padding:14px 16px;margin:12px 0;page-break-inside:avoid}
.row{display:flex;align-items:center;gap:10px;margin:5px 0}.row .lab{width:120px;font-weight:600}
.bar{flex:1;height:10px;background:#EEF0F6;border-radius:6px;overflow:hidden}.bar i{display:block;height:100%;background:linear-gradient(90deg,#7C8CE6,#4B3FA8)}
.val{width:52px;text-align:right;font-variant-numeric:tabular-nums}
table{border-collapse:collapse;width:100%;font-size:.9rem}th,td{text-align:left;padding:5px 8px;border-bottom:1px solid #EEF0F6}th{color:#6b7280;font-size:.75rem;text-transform:uppercase}
.tag{display:inline-block;padding:1px 8px;border-radius:999px;font-size:.75rem;font-weight:700}
.strong{background:#D1FAE5;color:#065F46}.moderate{background:#FEF3C7;color:#92400E}.short{background:#FEE2E2;color:#991B1B}
.note{color:#6b7280;font-size:.85rem}.low{background:#FEF3C7;color:#92400E;padding:1px 8px;border-radius:999px;font-size:.75rem}
@media print{body{padding:0}}"

#' Build the personal report as a complete HTML page.
#'
#' @param rec list from compute_recommendation().
#' @param X full decision matrix (all States/UTs), used for similar States/UTs.
#' @param low_conf names of low-confidence States/UTs.
#' @param source how the answers were produced: "questions", "equal", "edited"
#'   or "preset: <name>".
#' @param state_type named vector: State/UT name -> "State" or "UT".
#' @param shortlist optional States/UTs the user chose to compare.
#' @param scenario optional compute_recommendation() result for a what-if scenario.
#' @param pareto optional pareto_front() result over all seven criteria.
#' @return a character string (HTML).
build_report_html <- function(rec, X, low_conf = character(0), source = "questions",
                              state_type = NULL, shortlist = NULL, scenario = NULL, pareto = NULL) {
  tg <- htmltools::tags
  res <- rec$topsis$results[order(rec$topsis$results$Rank), ]
  w <- rec$ahp$weights
  wt <- weights_table(rec$ahp)
  n <- nrow(res)
  top <- res[seq_len(min(3, n)), ]
  kind <- function(st) if (is.null(state_type)) "" else state_type[[st]]
  
  bar <- function(frac, label, value) {
    tg$div(class = "row", tg$span(class = "lab", label),
           tg$div(class = "bar", tg$i(style = sprintf("width:%.0f%%", 100 * frac))),
           tg$span(class = "val", value))
  }
  mode <- if (source == "equal") "Equal weights (questions skipped)"
  else if (startsWith(source, "preset:")) paste0("Started from the ", sub("^preset: ", "", source), " preset")
  else "Your own answers"
  
  top_cards <- lapply(seq_len(nrow(top)), function(i) {
    st <- top$State[i]
    pb <- priority_breakdown(rec$topsis, w, st)
    sim <- similar_states(X, st, 3)
    tg$div(class = "card",
           tg$h3(sprintf("%d. %s", i, st), tg$span(class = "note", sprintf(" (%s, closeness %.3f)", kind(st), top$Closeness[i])),
                 if (st %in% low_conf) tg$span(class = "low", " low data confidence")),
           lapply(seq_len(nrow(pb)), function(j) {
             cls <- c("Strong match" = "strong", "Moderate match" = "moderate", "Falls short" = "short")[[pb$Verdict[j]]]
             tg$div(bar(pb$Score[j], pb$Criterion[j], sprintf("%.2f", pb$Score[j])),
                    tg$p(class = "note", tg$span(class = paste("tag", cls), pb$Verdict[j]), " ", pb$Sentence[j]))
           }),
           tg$p(shortfall_sentence(rec$topsis, w, st)),
           tg$p(class = "note", "Similar States/UTs (same overall profile): ",
                paste(sprintf("%s (%.2f)", sim$State, sim$Distance), collapse = ", ")))
  })
  
  # Optional sections. Their table rows carry a class, so the ranking table is
  # the only one written as plain <tr><td> rows (analysis/09_report_check.R counts them).
  sec <- 3
  next_sec <- function(title) { sec <<- sec + 1; tg$h2(sprintf("%d. %s", sec, title)) }
  shortlist <- intersect(shortlist, rownames(rec$X))
  compare_part <- if (length(shortlist) > 0) {
    htmltools::tagList(next_sec("My comparison"),
            tg$table(tg$thead(tg$tr(tg$th("Criterion"), lapply(shortlist, tg$th))),
                     tg$tbody(lapply(wt$Criterion, function(cn) {
                       tg$tr(class = "cmp", tg$td(sprintf("%s (%.0f%%)", cn, 100 * w[[cn]])),
                             lapply(shortlist, function(st) tg$td(sprintf("%.2f", rec$X[st, cn]))))
                     }),
                     tg$tr(class = "cmp", tg$td(tg$strong("Match score (rank)")),
                           lapply(shortlist, function(st) {
                             r <- res[res$State == st, ]
                             tg$td(tg$strong(sprintf("%.3f (%d)", r$Closeness, r$Rank)))
                           })))))
  }
  scenario_part <- if (!is.null(scenario)) {
    # The scenario may rank a different set of States/UTs (for example if the
    # low-confidence switch differs), so the current rank is looked up by name
    # and shown as "not ranked" where it does not exist.
    sres <- scenario$topsis$results[order(scenario$topsis$results$Rank), ]
    top5 <- head(data.frame(Personal_Rank = sres$Rank, State = sres$State,
                            Baseline_Rank = res$Rank[match(sres$State, res$State)],
                            stringsAsFactors = FALSE), 5)
    top5$Baseline_Rank <- ifelse(is.na(top5$Baseline_Rank), "not ranked", as.character(top5$Baseline_Rank))
    same_set <- setequal(sres$State, res$State)
    swt <- weights_table(scenario$ahp)
    htmltools::tagList(next_sec("What-if scenario (hypothetical)"),
            tg$p(class = "note", "Not your results above: a ranking for other ratings you tried. Weighted most: ",
                 paste(sprintf("%s %.0f%%", head(swt$Criterion, 3), 100 * head(swt$Weight, 3)), collapse = ", "), "."),
            tg$table(tg$thead(tg$tr(lapply(c("Scenario rank", "State/UT", "Current rank"), tg$th))),
                     tg$tbody(lapply(seq_len(nrow(top5)), function(i) {
                       tg$tr(class = "wi", tg$td(top5$Personal_Rank[i]), tg$td(top5$State[i]), tg$td(top5$Baseline_Rank[i]))
                     }))),
            if (!same_set) tg$p(class = "note", sprintf("The scenario ranks %d States/UTs and your results above rank %d, so the two lists cover different places.",
                                                       nrow(sres), n)))
  }
  pareto_part <- if (!is.null(pareto)) {
    eff <- pareto$State[which(pareto$Efficient)]
    htmltools::tagList(next_sec("Pareto-efficient places"),
            tg$p(sprintf("Pareto-efficient across all seven criteria (no other State/UT is at least as good on every criterion and better on one): %d of %d.",
                         length(eff), nrow(pareto))),
            tg$p(class = "note", paste(eff, collapse = ", ")),
            tg$p(class = "note", "Efficiency ignores your weights; it complements, and does not replace, the TOPSIS ranking."))
  }

  rank_rows <- lapply(seq_len(n), function(i) {
    st <- res$State[i]
    tg$tr(tg$td(res$Rank[i]), tg$td(st), tg$td(kind(st)), tg$td(sprintf("%.3f", res$Closeness[i])),
          tg$td(if (st %in% low_conf) "low confidence" else ""))
  })
  
  body <- tg$body(
    tg$h1("My Livability Report"),
    tg$p(class = "sub", sprintf("City Livability Optimizer | %s | generated %s", mode, format(Sys.Date(), "%d %B %Y"))),
    tg$h2("1. My priorities (AHP weights)"),
    lapply(seq_len(nrow(wt)), function(i) {
      bar(wt$Weight[i] / max(wt$Weight), wt$Criterion[i], sprintf("%.1f%%", 100 * wt$Weight[i]))
    }),
    tg$p(class = "note", consistency_message(rec$ahp)),
    tg$h2("2. My top 3"),
    top_cards,
    tg$h2(sprintf("3. Full ranking (%d States/UTs)", n)),
    tg$table(tg$thead(tg$tr(lapply(c("Rank", "State/UT", "Type", "Closeness", "Note"), tg$th))),
             tg$tbody(rank_rows)),
    if (length(rec$excluded) > 0) {
      tg$p(class = "note", "Not ranked (left out by choice): ", paste(rec$excluded, collapse = ", "))
    },
    compare_part, scenario_part, pareto_part,
    next_sec("How this was calculated"),
    tg$ol(lapply(METHOD_STEPS, tg$li)),
    next_sec("Limits to keep in mind"),
    tg$ul(lapply(LIMITATIONS, tg$li)),
    tg$h3("Notes on how the scores were built"),
    tg$ul(lapply(DATA_NOTES, tg$li)),
    tg$p(class = "note", "Programming for Data Science (BCSE207L) project."))
  # The <head> is written as plain text: htmltools would hoist a <head> tag out
  # of the page, which would drop the title and the styles.
  paste0("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
         "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
         "<title>My Livability Report</title>\n<style>", REPORT_CSS, "</style>\n</head>\n",
         as.character(body), "\n</html>\n")
}