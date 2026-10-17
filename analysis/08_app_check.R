# ==============================================================================
# Step 7a - Checks for the app's logic (run BEFORE launching the app)
# ------------------------------------------------------------------------------
# Purpose : Test everything the app calculates without needing a browser:
#           answers -> weights -> ranking, the "why this fits you" text, the
#           low-confidence switch, the map with excluded States, and that
#           app/app.R loads without errors.
# Run from: the project root (open the .Rproj file, then source this script).
# ==============================================================================

source(file.path("R", "common.R"))
source(file.path("R", "ahp.R"))
source(file.path("R", "topsis.R"))
source(file.path("R", "similarity.R"))
source(file.path("R", "explain.R"))
source(file.path("R", "map.R"))
source(file.path("R", "app_logic.R"))

d <- load_livability_data()
X <- criteria_matrix(d)
low_conf <- d$State[d$Low_Confidence == 1]
cat("\n=== STEP 7a: APP LOGIC CHECKS ===\n")

n_pass <- 0; n_fail <- 0
check <- function(label, ok) {
  ok <- isTRUE(ok)
  if (ok) n_pass <<- n_pass + 1 else n_fail <<- n_fail + 1
  cat(sprintf("[%s] %s\n", ifelse(ok, "PASS", "FAIL"), label))
}
top3 <- function(rec) top_k(rec$topsis, 3)$State

# ---- 1. Questions ------------------------------------------------------------
cat("\n--- 1. Flashcard questions ---\n")
check("There is one question per criterion, in the project's criterion order",
      nrow(QUESTIONS) == 7 && identical(QUESTIONS$Criterion, CRITERIA))
check("Every question has an icon and a hint",
      all(nzchar(QUESTIONS$Icon)) && all(nzchar(QUESTIONS$Hint)))
check("There are five answer labels for the 1-5 buttons", length(ANSWER_LABELS) == 5)

# ---- 2. Answers -> recommendation (must match Steps 3 and 4) ------------------
cat("\n--- 2. Answers -> weights -> ranking ---\n")
expected <- list(`Young Professional` = c("Delhi", "Chandigarh", "Goa"),
                 Family = c("Himachal Pradesh", "Chandigarh", "Punjab"),
                 Retiree = c("Lakshadweep", "Ladakh", "Karnataka"))
for (nm in names(expected)) {
  rec <- compute_recommendation(setNames(as.integer(PERSONAS[[nm]]), CRITERIA), X, low_conf)
  cat(sprintf("%-20s %s\n", nm, paste(top3(rec), collapse = " > ")))
  check(paste("Same top 3 as Step 4 for", nm), identical(top3(rec), expected[[nm]]))
}
eq <- compute_recommendation(EQUAL_ANSWERS, X, low_conf)
check("Skipping the questions gives equal weights (1/7 each) and CR = 0",
      all(abs(eq$ahp$weights - 1 / 7) < 1e-12) && abs(eq$ahp$CR) < 1e-12)
check("Equal-weight top 3 matches Step 4 (Chandigarh, Himachal Pradesh, Goa)",
      identical(top3(eq), c("Chandigarh", "Himachal Pradesh", "Goa")))
bad <- EQUAL_ANSWERS; bad[3] <- NA
check("Incomplete answers are rejected with an error",
      inherits(try(compute_recommendation(bad, X, low_conf), silent = TRUE), "try-error"))
fam <- compute_recommendation(setNames(as.integer(PERSONAS[["Family"]]), CRITERIA), X, low_conf)
wt <- weights_table(fam$ahp)
check("Weights table is sorted largest first and sums to 1",
      !is.unsorted(rev(round(wt$Weight, 12))) && abs(sum(wt$Weight) - 1) < 1e-12)
check("Equally rated criteria keep the criterion order (no floating-point tie noise)",
      identical(wt$Criterion[1:3], c("Education", "Safety", "Health")))
check("Consistency message reports 'consistent' for a normal set of answers",
      grepl("consistent", consistency_message(fam$ahp)))

# ---- 3. Leaving out low-confidence States/UTs ---------------------------------
cat("\n--- 3. Low-confidence switch ---\n")
ret_in  <- compute_recommendation(setNames(as.integer(PERSONAS[["Retiree"]]), CRITERIA), X, low_conf, FALSE)
ret_out <- compute_recommendation(setNames(as.integer(PERSONAS[["Retiree"]]), CRITERIA), X, low_conf, TRUE)
cat("Retiree with all 36 :", paste(top3(ret_in), collapse = " > "), "\n")
cat("Retiree without the 4 low-confidence:", paste(top3(ret_out), collapse = " > "), "\n")
check("Excluding removes exactly the 4 low-confidence States/UTs (32 ranked)",
      nrow(ret_out$X) == 32 && setequal(ret_out$excluded, low_conf))
check("No excluded State/UT appears in the ranking",
      !any(low_conf %in% ret_out$topsis$results$State))
check("Ranks after exclusion run 1 to 32", identical(sort(ret_out$topsis$results$Rank), 1:32))
check("The Retiree top 3 changes once low-confidence States/UTs are left out",
      !identical(top3(ret_in), top3(ret_out)))

# ---- 4. "Why this fits you" ---------------------------------------------------
cat("\n--- 4. Why this fits you ---\n")
pb <- priority_breakdown(fam$topsis, fam$ahp$weights, "Himachal Pradesh")
cat(pb$Sentence[1], "\n")
check("Breakdown has 3 rows with the user's 3 highest-weighted criteria",
      nrow(pb) == 3 && setequal(pb$Criterion, names(sort(fam$ahp$weights, decreasing = TRUE))[1:3]))
check("Scores in the breakdown are the real scores from the data",
      all(abs(pb$Score - X["Himachal Pradesh", pb$Criterion]) < 1e-12))
check("Weights in the breakdown are the user's real AHP weights",
      all(abs(pb$Weight - fam$ahp$weights[pb$Criterion]) < 1e-12))
check("Sentence quotes the weight percentage and the score",
      grepl("26%", pb$Sentence[1], fixed = TRUE) && grepl("0.83/1.00", pb$Sentence[1], fixed = TRUE))
safety_first <- compute_recommendation(setNames(as.integer(c(1, 5, 1, 1, 1, 1, 1)), CRITERIA), X, low_conf)
pb_delhi <- priority_breakdown(safety_first$topsis, safety_first$ahp$weights, "Delhi", 1)
check("A weak score on a top priority is reported as 'Falls short' (Delhi, safety first)",
      pb_delhi$Verdict == "Falls short" && grepl("falls short", pb_delhi$Sentence))
check("A strong score is reported as 'Strong match' (Himachal Pradesh, education)",
      pb$Verdict[pb$Criterion == "Education"] == "Strong match")
yp <- compute_recommendation(setNames(as.integer(PERSONAS[["Young Professional"]]), CRITERIA), X, low_conf)
check("Shortfall sentence names Safety for Delhi (its weakest criterion)",
      grepl("furthest short: Safety", shortfall_sentence(yp$topsis, yp$ahp$weights, "Delhi")))
check("The explanation changes when the user's answers change",
      !identical(priority_breakdown(fam$topsis, fam$ahp$weights, "Chandigarh")$Sentence,
                 priority_breakdown(yp$topsis, yp$ahp$weights, "Chandigarh")$Sentence))

# ---- 5. Map with excluded States/UTs -------------------------------------------
cat("\n--- 5. Map ---\n")
html <- htmltools::renderTags(render_india_map(ret_out$topsis$results, X, low_conf))$html
n_excl <- lengths(regmatches(html, gregexpr("Excluded from this ranking", html, fixed = TRUE)))[1]
check("Excluded States/UTs stay on the map and say 'Excluded from this ranking' (4 outlines + 3 markers)",
      n_excl == 7)
check("Excluded States/UTs are drawn in plain grey",
      lengths(regmatches(html, gregexpr('fill="#E5E7EB"', html, fixed = TRUE)))[1] == 7)
check("A State name that is not on the map is still rejected",
      inherits(try(render_india_map(transform(fam$topsis$results,
                                              State = ifelse(State == "Goa", "Goaa", State)),
                                    X, low_conf), silent = TRUE), "try-error"))

# ---- 6. The app file itself ----------------------------------------------------
cat("\n--- 6. App file ---\n")
check("app/www/style.css and the logo exist", all(file.exists(file.path("app", "www", c("style.css", "img/logo.svg")))))
app <- local({
  old <- setwd("app"); on.exit(setwd(old))
  tryCatch(source("app.R", local = new.env())$value, error = function(e) e)
})
if (inherits(app, "error")) cat("App failed to load:", conditionMessage(app), "\n")
check("app/app.R loads without errors and builds a Shiny app", inherits(app, "shiny.appobj"))

cat(sprintf("\nChecks passed: %d   failed: %d\n", n_pass, n_fail))
cat("\nNext: open app/app.R and click 'Run App'.\n")
cat("\n=== Step 7a checks complete ===\n")