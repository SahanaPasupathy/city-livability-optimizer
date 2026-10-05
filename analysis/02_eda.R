# ==============================================================================
# Step 2 - Focused exploratory data analysis (EDA)
# ------------------------------------------------------------------------------
# Purpose : Understand the seven criterion scores before weighting them.
# Input   : data/State_UT_Livability_Dataset_Final.csv (read only)
# Output  : printed summaries, 4 figures (outputs/figures), 3 tables
#           (outputs/tables).
# Run from: the project root (open the .Rproj file, then source this script).
# ==============================================================================

library(ggplot2)
source(file.path("R", "common.R"))

d <- load_livability_data()
X <- criteria_matrix(d)
cat("\n=== STEP 2: EXPLORATORY DATA ANALYSIS ===\n")
cat("States/UTs:", nrow(d), "| Criteria:", length(CRITERIA), "\n")

# ---- 1. Summary statistics of the seven criteria ----------------------------
cat("\n--- 1. Summary statistics (scores run 0 = worst to 1 = best) ---\n")
summary_tbl <- data.frame(
  Criterion = CRITERIA,
  Mean   = apply(X, 2, mean),
  Median = apply(X, 2, median),
  SD     = apply(X, 2, sd),
  Min    = apply(X, 2, min),
  Max    = apply(X, 2, max),
  row.names = NULL
)
print(summary_tbl, digits = 3, row.names = FALSE)
save_table(summary_tbl, "02_criteria_summary.csv")

# ---- 2. Missingness ---------------------------------------------------------
cat("\n--- 2. Missing raw indicator values ---\n")
na_mat <- is.na(d[INDICATORS])
rownames(na_mat) <- d$State
cat("Total missing cells:", sum(na_mat), "of", length(na_mat), "\n")
miss_by_state <- sort(rowSums(na_mat), decreasing = TRUE)
miss_by_state <- miss_by_state[miss_by_state > 0]
cat("States/UTs with missing values (count):\n")
print(miss_by_state)

keep_rows <- rowSums(na_mat) > 0
keep_cols <- colSums(na_mat) > 0
na_sub    <- na_mat[keep_rows, keep_cols, drop = FALSE]
na_long <- data.frame(
  State     = factor(rep(rownames(na_sub), times = ncol(na_sub)),
                     levels = rev(names(miss_by_state))),
  Indicator = rep(colnames(na_sub), each = nrow(na_sub)),
  Status    = ifelse(as.vector(na_sub), "Missing", "Present")
)
p_missing <- ggplot(na_long, aes(Indicator, State, fill = Status)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  scale_fill_manual(values = c(Present = "grey88", Missing = "#C0392B")) +
  labs(title = "Missing raw indicator values",
       subtitle = "Only States/UTs and indicators with at least one gap are shown",
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_figure(p_missing, "02_missingness.png", width = 8, height = 4.5)

# ---- 3. Distribution of the criterion scores --------------------------------
cat("\n--- 3. Distribution plot ---\n")
scores_long <- data.frame(
  State     = rep(d$State, times = length(CRITERIA)),
  Criterion = factor(rep(CRITERIA, each = nrow(d)), levels = CRITERIA),
  Score     = as.vector(X)
)
p_dist <- ggplot(scores_long, aes(Criterion, Score)) +
  geom_boxplot(fill = "#AED6F1", outlier.shape = NA, width = 0.55) +
  geom_jitter(width = 0.12, height = 0, alpha = 0.7, size = 1.6,
              colour = "#1B4F72") +
  labs(title = "Distribution of the seven criterion scores",
       subtitle = "Each dot is one State/UT; 1 = best, 0 = worst",
       x = NULL, y = "Criterion score") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_figure(p_dist, "02_criteria_distribution.png", width = 8, height = 5)

# ---- 4. Correlation between criteria ----------------------------------------
cat("\n--- 4. Correlation between criteria (Spearman) ---\n")
rho <- cor(X, method = "spearman")
print(round(rho, 2))
pairs <- which(upper.tri(rho), arr.ind = TRUE)
pair_tbl <- data.frame(A = CRITERIA[pairs[, 1]], B = CRITERIA[pairs[, 2]],
                       Rho = rho[pairs])
pair_tbl <- pair_tbl[order(-abs(pair_tbl$Rho)), ]
cat("\nStrongest relationships:\n")
print(head(pair_tbl, 3), digits = 2, row.names = FALSE)
cat("Largest absolute correlation:", round(max(abs(pair_tbl$Rho)), 2),
    "(below 0.7 means no two criteria are near-duplicates)\n")

rho_long <- data.frame(
  A   = factor(rep(CRITERIA, times = length(CRITERIA)), levels = CRITERIA),
  B   = factor(rep(CRITERIA, each = length(CRITERIA)), levels = rev(CRITERIA)),
  Rho = as.vector(rho)
)
p_corr <- ggplot(rho_long, aes(A, B, fill = Rho)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", Rho)), size = 3.4) +
  scale_fill_gradient2(low = "#C0392B", mid = "white", high = "#2471A3",
                       limits = c(-1, 1)) +
  labs(title = "Correlation between the seven criteria (Spearman)",
       x = NULL, y = NULL, fill = "rho") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_figure(p_corr, "02_criteria_correlation.png", width = 7, height = 5.5)

# ---- 5. Strongest and weakest States/UTs per criterion ----------------------
cat("\n--- 5. Strongest and weakest 3 States/UTs for each criterion ---\n")
extremes <- do.call(rbind, lapply(CRITERIA, function(cr) {
  ord <- order(X[, cr], decreasing = TRUE)
  best  <- ord[1:3]
  worst <- rev(tail(ord, 3))
  data.frame(
    Criterion = cr,
    Position  = rep(c("Strongest", "Weakest"), each = 3),
    Place     = rep(1:3, times = 2),
    State     = rownames(X)[c(best, worst)],
    Score     = X[c(best, worst), cr],
    row.names = NULL
  )
}))
for (cr in CRITERIA) {
  e <- extremes[extremes$Criterion == cr, ]
  cat(sprintf("%-12s best: %s | worst: %s\n", cr,
              paste0(e$State[1:3], " (", round(e$Score[1:3], 2), ")", collapse = ", "),
              paste0(e$State[4:6], " (", round(e$Score[4:6], 2), ")", collapse = ", ")))
}
save_table(extremes, "02_criterion_extremes.csv")

# ---- 6. Equal-weight baseline ranking ---------------------------------------
cat("\n--- 6. Equal-weight baseline ranking ---\n")
base <- d[order(d$Baseline_Rank),
          c("Baseline_Rank", "State", "Type", "Baseline_Score_EqualWeights",
            "Low_Confidence")]
cat("Top 10:\n");    print(head(base, 10), row.names = FALSE)
cat("Bottom 5:\n");  print(tail(base, 5),  row.names = FALSE)
cat("Low-confidence States/UTs and their baseline rank:\n")
print(base[base$Low_Confidence == 1, c("Baseline_Rank", "State")], row.names = FALSE)
save_table(base, "02_baseline_ranking.csv")

base$State <- factor(base$State, levels = rev(base$State))
base$Confidence <- ifelse(base$Low_Confidence == 1,
                          "Low confidence (missing data)", "Normal")
p_base <- ggplot(base, aes(State, Baseline_Score_EqualWeights, fill = Confidence)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(Normal = "#2E86C1",
                               `Low confidence (missing data)` = "#E67E22")) +
  labs(title = "Equal-weight baseline score of all 36 States/UTs",
       subtitle = "Mean of the seven criterion scores; this is NOT the personalised ranking",
       x = NULL, y = "Baseline score", fill = NULL) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "bottom")
save_figure(p_base, "02_baseline_ranking.png", width = 7.5, height = 8)

cat("\n=== EDA complete ===\n")