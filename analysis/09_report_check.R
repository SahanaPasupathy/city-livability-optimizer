# ==============================================================================
# Step 7b - Checks for the personal report and the explanation texts
# Run from the project root (open the .Rproj file, then source this script).
# Output: outputs/reports/09_sample_report_family.html (open it in a browser).
# ==============================================================================

source(file.path("R", "common.R"))
source(file.path("R", "ahp.R"))
source(file.path("R", "topsis.R"))
source(file.path("R", "similarity.R"))
source(file.path("R", "explain.R"))
source(file.path("R", "app_logic.R"))
source(file.path("R", "app_text.R"))
source(file.path("R", "report.R"))

d <- load_livability_data()
X <- criteria_matrix(d)
low_conf <- d$State[d$Low_Confidence == 1]
type <- setNames(d$Type, d$State)
cat("\n=== STEP 7b: REPORT AND TEXT CHECKS ===\n")

n_pass <- 0; n_fail <- 0
check <- function(label, ok) {
  ok <- isTRUE(ok)
  if (ok) n_pass <<- n_pass + 1 else n_fail <<- n_fail + 1
  cat(sprintf("[%s] %s\n", ifelse(ok, "PASS", "FAIL"), label))
}
has <- function(html, txt) grepl(txt, html, fixed = TRUE)
count <- function(html, txt) lengths(regmatches(html, gregexpr(txt, html, fixed = TRUE)))[1]
rows <- function(html) lengths(regmatches(html, gregexpr("<tr>\\s*<td>", html)))[1]

cat("\n--- 1. Shared texts ---\n")
check("Every criterion has a description of its indicators",
      setequal(names(CRITERION_INDICATORS), CRITERIA))
check("The method has 8 steps and the limits list has 7 points",
      length(METHOD_STEPS) == 8 && length(LIMITATIONS) == 7)
check("The data caveats in the text match the dataset (Manipur 1.32, 4 low-confidence)",
      round(d$Avg_Inflation_PriceStability[d$State == "Manipur"], 2) == 1.32 &&
        length(low_conf) == 4 && all(vapply(low_conf, function(s) any(grepl(s, LIMITATIONS, fixed = TRUE)), logical(1))))

cat("\n--- 2. Family report ---\n")
ans <- setNames(as.integer(PERSONAS[["Family"]]), CRITERIA)
rec <- compute_recommendation(ans, X, low_conf)
html <- build_report_html(rec, X, low_conf, "preset: Family", type)
dir.create(file.path("outputs", "reports"), showWarnings = FALSE, recursive = TRUE)
f <- file.path("outputs", "reports", "09_sample_report_family.html")
writeLines(html, f, useBytes = TRUE)
cat("Saved report:", f, "\n")
check("Report is a complete HTML page", startsWith(html, "<!DOCTYPE html>") && has(html, "</html>"))
check("Report has a title and its styles in the <head>",
      has(html, "<title>My Livability Report</title>") && has(html, "<style>") && has(html, "</head>"))
check("Report names the top 3 (Himachal Pradesh, Chandigarh, Punjab)",
      all(vapply(c("1. Himachal Pradesh", "2. Chandigarh", "3. Punjab"), has, logical(1), html = html)))
check("Report shows all 7 weights with the real percentages",
      all(vapply(sprintf("%.1f%%", 100 * rec$ahp$weights), function(p) has(html, p), logical(1))))
check("Report lists all 36 States/UTs in the full ranking", rows(html) == 36)
check("Report includes the consistency message", has(html, "Consistency ratio"))
check("Report includes 'why this fits you' sentences and a shortfall line",
      has(html, "carries 26% of your preference weight") && has(html, "Where it falls furthest short"))
check("Report includes similar States/UTs, the method and the limits",
      has(html, "Similar States/UTs") && has(html, "Pairwise comparisons") && has(html, "Manipur"))
check("Chandigarh is flagged as low confidence", has(html, "low data confidence"))

cat("\n--- 3. Other modes ---\n")
eq <- compute_recommendation(EQUAL_ANSWERS, X, low_conf)
h_eq <- build_report_html(eq, X, low_conf, "equal", type)
check("Equal-weights report says so and shows 14.3% weights",
      has(h_eq, "Equal weights (questions skipped)") && has(h_eq, "14.3%"))
ex <- compute_recommendation(PERSONAS[["Retiree"]], X, low_conf, TRUE)
h_ex <- build_report_html(ex, X, low_conf, "questions", type)
check("Report with exclusion ranks 32 and names the 4 left out",
      rows(h_ex) == 32 && has(h_ex, "Not ranked (left out by choice)") && has(h_ex, "Ladakh"))
check("Report with exclusion shows Karnataka first", has(h_ex, "1. Karnataka"))
tricky <- X; rownames(tricky)[1] <- "A<b>&Co"
check("Special characters in names are escaped, not injected as HTML",
      {rt <- compute_recommendation(EQUAL_ANSWERS, tricky, character(0))
      h <- build_report_html(rt, tricky, character(0), "equal", NULL)
      !has(h, "A<b>&Co") && has(h, "A&lt;b&gt;&amp;Co")})

cat(sprintf("\nChecks passed: %d   failed: %d\n", n_pass, n_fail))
cat("\nOpen outputs/reports/09_sample_report_family.html in a browser to see the report.\n")
cat("\n=== Step 7b checks complete ===\n")