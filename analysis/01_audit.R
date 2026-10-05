# ==============================================================================
# Step 1 - Dataset audit
# ------------------------------------------------------------------------------
# Purpose : Verify the existing dataset BEFORE using it in AHP/TOPSIS.
#           The raw CSV is only read, never modified.
# Input   : data/State_UT_Livability_Dataset_Final.csv
# Output  : printed PASS/FAIL report + outputs/tables/01_audit_checks.csv
# Run from: the project root (open the .Rproj file, then source this script).
# ==============================================================================

data_path <- file.path("data", "State_UT_Livability_Dataset_Final.csv")
if (!file.exists(data_path)) {
  stop("Dataset not found at '", data_path, "'. Open the RStudio project ",
       "(.Rproj) so the working directory is the project root.", call. = FALSE)
}

# ---- Expected structure ------------------------------------------------------
id_cols    <- c("State", "Type", "Region")
indicators <- c(
  "Education_Score", "Grade8_Proficiency_Pct", "HigherSec_GER_Pct",
  "Crime_Rate", "Murders_per_Lakh",
  "Immunisation_Pct", "Institutional_Deliveries_Pct", "Health_Worker_Density",
  "Kachha_Houses_Pct", "Clean_Cooking_Fuel_Pct", "Rural_Piped_Water_Pct",
  "Unemployment_Rate_Pct", "MPI_Headcount_Pct", "Avg_Inflation_PriceStability",
  "AirQuality_PM25",
  "Villages_3G4G_Pct", "EVs_per_Lakh"
)
quality_cols <- c("Missing_Subindicators", "Low_Confidence")
criteria     <- c("Education", "Safety", "Health", "Housing",
                  "Economy", "Environment", "Connectivity")
norm_cols    <- paste0(criteria, "_norm")
baseline_cols <- c("Baseline_Score_EqualWeights", "Baseline_Rank")
expected_cols <- c(id_cols, indicators, quality_cols, norm_cols, baseline_cols)

# For every raw indicator: which criterion it feeds, and whether HIGHER raw
# values should mean BETTER livability (+1) or WORSE (-1).
direction <- data.frame(
  indicator = indicators,
  criterion = c(rep("Education", 3), rep("Safety", 2), rep("Health", 3),
                rep("Housing", 3), rep("Economy", 3), "Environment",
                rep("Connectivity", 2)),
  expected_sign = c(1, 1, 1,  -1, -1,  1, 1, 1,  -1, 1, 1,  -1, -1, -1,
                    -1,  1, 1),
  stringsAsFactors = FALSE
)

# ---- Helpers to record and print results ------------------------------------
results <- data.frame(Check = character(), Status = character(),
                      Detail = character(), stringsAsFactors = FALSE)

# `<<-` updates the `results` table that lives outside this function.
record <- function(check, passed, detail = "", info_only = FALSE) {
  status <- if (info_only) "INFO" else if (isTRUE(passed)) "PASS" else "FAIL"
  results[nrow(results) + 1, ] <<- list(check, status, detail)
  cat(sprintf("[%s] %s%s\n", status, check,
              if (nzchar(detail)) paste0("  -  ", detail) else ""))
}

# ---- Load -------------------------------------------------------------------
d <- read.csv(data_path, na.strings = "NA", stringsAsFactors = FALSE)
cat("\n=== STEP 1: DATASET AUDIT ===\n\n")

# ---- 1-3. Structure ---------------------------------------------------------
cat("--- Structure ---\n")
record("1. Number of rows is 36", nrow(d) == 36, paste("rows =", nrow(d)))
record("2. Number of columns is 31", ncol(d) == 31, paste("columns =", ncol(d)))
missing_cols <- setdiff(expected_cols, names(d))
extra_cols   <- setdiff(names(d), expected_cols)
record("3. Column names match the plan",
       length(missing_cols) == 0 && length(extra_cols) == 0,
       paste0("missing: [", paste(missing_cols, collapse = ", "),
              "]  unexpected: [", paste(extra_cols, collapse = ", "), "]"))
if (length(missing_cols) > 0) {
  stop("Cannot continue: required columns are missing.", call. = FALSE)
}

# ---- 4. Data types ----------------------------------------------------------
numeric_cols <- c(indicators, quality_cols, norm_cols, baseline_cols)
non_numeric  <- numeric_cols[!sapply(d[numeric_cols], is.numeric)]
record("4. All indicator/score columns are numeric",
       length(non_numeric) == 0,
       paste("non-numeric:", if (length(non_numeric)) paste(non_numeric, collapse = ", ") else "none"))
record("4b. State, Type, Region are text",
       all(sapply(d[id_cols], is.character)))

# ---- 5. Duplicates and name hygiene ----------------------------------------
cat("\n--- Names and duplicates ---\n")
record("5. No duplicate States/UTs", !any(duplicated(d$State)),
       paste("duplicates =", sum(duplicated(d$State))))
record("5b. No stray spaces in State names", all(d$State == trimws(d$State)))
record("5c. Type has 28 States and 8 UTs",
       sum(d$Type == "State") == 28 && sum(d$Type == "UT") == 8,
       paste("States =", sum(d$Type == "State"), " UTs =", sum(d$Type == "UT")))

# ---- 6. Missing values ------------------------------------------------------
cat("\n--- Missing values ---\n")
na_per_col <- colSums(is.na(d))
cat("Columns containing NA (raw indicators only; scores must have none):\n")
print(na_per_col[na_per_col > 0])
record("6. No missing values in criterion scores or baseline",
       sum(na_per_col[c(norm_cols, baseline_cols)]) == 0)
record("6b. Total missing raw indicator cells",
       TRUE, paste(sum(na_per_col[indicators]), "of", nrow(d) * length(indicators),
                   "cells (kept as NA, not imputed)"), info_only = TRUE)

# ---- 7-8. Data-quality flags ------------------------------------------------
cat("\n--- Data-quality columns ---\n")
recount <- rowSums(is.na(d[indicators]))
record("7. Missing_Subindicators equals the real count of NAs per row",
       all(recount == d$Missing_Subindicators))
record("8. Low_Confidence contains only 0 or 1", all(d$Low_Confidence %in% c(0, 1)))
rule_flag <- as.integer(d$Missing_Subindicators >= 3 | d$State == "Lakshadweep")
record("8b. Low_Confidence follows the rule (>= 3 missing, or Lakshadweep)",
       all(rule_flag == d$Low_Confidence))
cat("Low-confidence states/UTs:",
    paste(d$State[d$Low_Confidence == 1], collapse = ", "), "\n")

# ---- 9. Ranges of every numeric variable -----------------------------------
cat("\n--- Ranges of all numeric variables ---\n")
rng <- data.frame(
  Variable = numeric_cols,
  Min  = sapply(d[numeric_cols], min, na.rm = TRUE),
  Max  = sapply(d[numeric_cols], max, na.rm = TRUE),
  Mean = sapply(d[numeric_cols], mean, na.rm = TRUE),
  row.names = NULL
)
print(rng, digits = 4, row.names = FALSE)
pct_cols <- grep("_Pct$", indicators, value = TRUE)
over100  <- sapply(d[pct_cols], function(x) sum(x > 100, na.rm = TRUE))
over100  <- over100[over100 > 0]
record("9. Percentage indicators above 100 (reported by source)",
       TRUE, paste(names(over100), over100, sep = ": ", collapse = "; "),
       info_only = TRUE)

# ---- 10-11. Normalised scores ----------------------------------------------
cat("\n--- Normalised criterion scores ---\n")
norm_mat <- as.matrix(d[norm_cols])
record("10. All seven _norm columns lie between 0 and 1",
       all(norm_mat >= 0 & norm_mat <= 1))
record("11. Each _norm column spans exactly 0 to 1 (min = 0, max = 1)",
       all(abs(apply(norm_mat, 2, min)) < 1e-6) &&
         all(abs(apply(norm_mat, 2, max) - 1) < 1e-6))

# ---- 12-13. Direction: higher score must mean better ------------------------
cat("\n--- Direction check (Spearman: raw indicator vs its criterion score) ---\n")
cat("Higher-is-better indicators must correlate POSITIVELY, lower-is-better NEGATIVELY.\n")
direction$rho <- mapply(function(ind, crit) {
  cor(d[[ind]], d[[paste0(crit, "_norm")]], method = "spearman",
      use = "complete.obs")
}, direction$indicator, direction$criterion)
direction$sign_ok <- sign(direction$rho) == direction$expected_sign
print(data.frame(Indicator = direction$indicator, Criterion = direction$criterion,
                 Expected = ifelse(direction$expected_sign > 0, "higher=better", "lower=better"),
                 Spearman = round(direction$rho, 2),
                 OK = ifelse(direction$sign_ok, "yes", "NO")),
      row.names = FALSE)
record("12. Higher criterion score = better, for every indicator",
       all(direction$sign_ok),
       paste(sum(direction$sign_ok), "of", nrow(direction), "indicators have the expected sign"))
rev_ok <- direction$sign_ok[direction$expected_sign < 0]
record("13. Lower-is-better indicators were correctly reversed",
       all(rev_ok), paste(sum(rev_ok), "of", length(rev_ok), "correct"))

# ---- 14. Are criterion scores sensible? -------------------------------------
cat("\n--- Criterion score sanity ---\n")
sds <- apply(norm_mat, 2, sd)
record("14. Every criterion varies across states (SD > 0.1)",
       all(sds > 0.1), paste(paste0(criteria, "=", round(sds, 2)), collapse = ", "))

# ---- 15-16. Equal-weight baseline -------------------------------------------
cat("\n--- Equal-weight baseline ---\n")
recomputed_score <- rowMeans(norm_mat)
max_gap <- max(abs(recomputed_score - d$Baseline_Score_EqualWeights))
record("15. Baseline_Score_EqualWeights = mean of the seven scores",
       max_gap < 1e-3, paste("largest difference =", signif(max_gap, 3),
                             "(scores are stored to 4 decimals)"))
recomputed_rank <- rank(-d$Baseline_Score_EqualWeights, ties.method = "min")
record("16. Baseline_Rank matches the ranking of the baseline score",
       all(recomputed_rank == d$Baseline_Rank))
ord <- order(d$Baseline_Rank)
cat("\nBaseline top 5:\n")
print(d[ord[1:5], c("Baseline_Rank", "State", "Baseline_Score_EqualWeights")], row.names = FALSE)
cat("Baseline bottom 5:\n")
print(d[tail(ord, 5), c("Baseline_Rank", "State", "Baseline_Score_EqualWeights")], row.names = FALSE)

# ---- Summary and saved report -----------------------------------------------
cat("\n=== SUMMARY ===\n")
print(table(results$Status))
dir.create(file.path("outputs", "tables"), showWarnings = FALSE, recursive = TRUE)
write.csv(results, file.path("outputs", "tables", "01_audit_checks.csv"),
          row.names = FALSE)
cat("Saved: outputs/tables/01_audit_checks.csv\n")
if (any(results$Status == "FAIL")) {
  cat("\nAt least one check FAILED - do not continue until it is understood.\n")
} else {
  cat("\nAll checks passed. The existing normalised scores can be reused.\n")
}