# ==============================================================================
# Step 4 - TOPSIS: ideal solutions, Euclidean distances, closeness, ranking
# ------------------------------------------------------------------------------
# Purpose : Validate the TOPSIS implementation, then rank all 36 States/UTs for
#           the three demo personas (AHP weights from Step 3) and for equal
#           weights.
# Input   : data/State_UT_Livability_Dataset_Final.csv, R/ahp.R, R/topsis.R
# Output  : printed validation + rankings, 1 table, 1 figure in outputs/
# Run from: the project root (open the .Rproj file, then source this script).
# ==============================================================================

library(ggplot2)
source(file.path("R", "common.R"))
source(file.path("R", "ahp.R"))
source(file.path("R", "topsis.R"))

d <- load_livability_data()
X <- criteria_matrix(d)
cat("\n=== STEP 4: TOPSIS ===\n")

# Small helper: print PASS/FAIL for one check and remember the result.
n_pass <- 0; n_fail <- 0
check <- function(label, ok) {
  ok <- isTRUE(ok)
  if (ok) n_pass <<- n_pass + 1 else n_fail <<- n_fail + 1
  cat(sprintf("[%s] %s\n", ifelse(ok, "PASS", "FAIL"), label))
}

# ---- 1. A tiny example we can solve by hand ---------------------------------
cat("\n--- 1. Hand-checkable example (3 alternatives, 2 criteria, equal weights) ---\n")
toy <- rbind(Best = c(1, 1), Middle = c(0.5, 0.5), Worst = c(0, 0))
colnames(toy) <- c("C1", "C2")
toy_res <- topsis(toy, c(0.5, 0.5))
print(toy_res$results, digits = 4, row.names = FALSE)
# By hand: ideal = (0.5, 0.5), negative ideal = (0, 0).
#  Best:   D+ = 0,      D- = 0.7071 -> C = 1
#  Middle: D+ = 0.3536, D- = 0.3536 -> C = 0.5
#  Worst:  D+ = 0.7071, D- = 0      -> C = 0
check("Toy example closeness equals the hand calculation (1, 0.5, 0)",
      all(abs(toy_res$results$Closeness - c(1, 0.5, 0)) < 1e-9))
check("Toy example ranks are 1, 2, 3", all(toy_res$results$Rank == 1:3))

# ---- 2. Step-by-step on the real data: Family persona -----------------------
cat("\n--- 2. Step-by-step on the real data (Family persona) ---\n")
fam_w   <- run_ahp(PERSONAS[["Family"]])$weights
fam     <- topsis(X, fam_w)
fam_res <- fam$results
cat("AHP weights used:\n"); print(round(fam_w, 4))
cat("\nDecision matrix: ", nrow(fam$decision), "States/UTs x", ncol(fam$decision),
    "criteria (the existing _norm scores, already scaled 0-1).\n")
cat("Normalisation used: none (scores are already min-max scaled; scaling again would double-scale).\n")
cat("\nPositive ideal (best weighted value per criterion - a reference point, not a State/UT):\n")
print(round(fam$ideal, 4))
cat("Negative ideal (worst weighted value per criterion):\n")
print(round(fam$anti_ideal, 4))

top_state <- fam_res$State[which.min(fam_res$Rank)]
cat("\nWorked example for the top-ranked State/UT:", top_state, "\n")
show <- rbind(Score = fam$decision[top_state, ],
              Weight = fam_w[colnames(X)],
              Weighted = fam$weighted[top_state, ],
              `Positive ideal` = fam$ideal,
              `Squared gap to ideal` = (fam$weighted[top_state, ] - fam$ideal)^2)
print(round(show, 4))
r <- fam_res[fam_res$State == top_state, ]
cat(sprintf("D+ = sqrt(sum of squared gaps to the positive ideal) = %.4f\n", r$D_plus))
cat(sprintf("D- = sqrt(sum of squared gaps to the negative ideal) = %.4f\n", r$D_minus))
cat(sprintf("C  = D- / (D+ + D-) = %.4f / (%.4f + %.4f) = %.4f\n",
            r$D_minus, r$D_plus, r$D_minus, r$Closeness))

# ---- 3. Validation checks on the real data ----------------------------------
cat("\n--- 3. Validation checks (Family persona on all 36 States/UTs) ---\n")
check("Decision matrix is 36 x 7",
      all(dim(fam$decision) == c(36, 7)))
check("Weighted matrix equals score x weight (recomputed independently)",
      max(abs(fam$weighted - t(t(X) * fam_w[colnames(X)]))) < 1e-12)
check("Positive ideal equals the weights (every criterion reaches 1)",
      max(abs(fam$ideal - fam_w[colnames(X)])) < 1e-12)
check("Negative ideal is all zeros (every criterion reaches 0)",
      max(abs(fam$anti_ideal)) < 1e-12)
# Recompute D+ and D- with explicit loops as an independent check.
loop_dp <- sapply(seq_len(nrow(X)), function(i) {
  sqrt(sum((fam$weighted[i, ] - fam$ideal)^2)) })
loop_dm <- sapply(seq_len(nrow(X)), function(i) {
  sqrt(sum((fam$weighted[i, ] - fam$anti_ideal)^2)) })
check("D+ and D- agree with a loop-based recalculation",
      max(abs(loop_dp - fam_res$D_plus), abs(loop_dm - fam_res$D_minus)) < 1e-12)
check("Closeness = D- / (D+ + D-) for every State/UT",
      max(abs(fam_res$Closeness - loop_dm / (loop_dp + loop_dm))) < 1e-12)
check("All closeness values lie between 0 and 1",
      all(fam_res$Closeness >= 0 & fam_res$Closeness <= 1))
check("Ranks 1..36 follow the closeness order",
      all(fam_res$Rank == rank(-fam_res$Closeness, ties.method = "min")) &&
        min(fam_res$Rank) == 1)
# If State i is at least as good as State j on every criterion (and better on
# one), TOPSIS must rank i above j.
violations <- 0; pairs <- 0
for (i in seq_len(nrow(X))) for (j in seq_len(nrow(X))) {
  if (i != j && all(X[i, ] >= X[j, ]) && any(X[i, ] > X[j, ])) {
    pairs <- pairs + 1
    if (!(fam_res$Closeness[i] > fam_res$Closeness[j])) violations <- violations + 1
  }
}
check(sprintf("Dominance: %d pairs where one State/UT beats another on every criterion - none ranked wrongly",
              pairs), violations == 0)
check("Same weights, same answer (function is deterministic)",
      identical(topsis(X, fam_w)$results, fam_res))
cat(sprintf("\nChecks passed: %d   failed: %d\n", n_pass, n_fail))

# ---- 4. Rankings for each persona and for equal weights ---------------------
cat("\n--- 4. Rankings ---\n")
weight_sets <- c(lapply(PERSONAS, function(a) run_ahp(a)$weights),
                 list(`Equal weights` = setNames(rep(1 / 7, 7), CRITERIA)))
rank_results <- lapply(weight_sets, function(w) topsis(X, w)$results)

for (nm in names(rank_results)) {
  rr <- rank_results[[nm]]
  rr$Type <- d$Type[match(rr$State, d$State)]
  rr$Flag <- ifelse(d$Low_Confidence[match(rr$State, d$State)] == 1, "low confidence", "")
  rr <- rr[order(rr$Rank), ]
  cat("\n", nm, " - top 10:\n", sep = "")
  print(rr[1:10, c("Rank", "State", "Closeness", "D_plus", "D_minus", "Flag")],
        digits = 3, row.names = FALSE)
  cat("Bottom 3:", paste0(tail(rr$State, 3), collapse = ", "), "\n")
}

cat("\nTop 3 by persona:\n")
for (nm in names(rank_results)) {
  rr <- rank_results[[nm]]
  cat(sprintf("  %-20s %s\n", nm, paste(rr$State[order(rr$Rank)][1:3], collapse = " > ")))
}

cat("\nSpearman correlation between rankings (1 = identical order):\n")
rank_mat <- sapply(rank_results, function(rr) rr$Rank[match(d$State, rr$State)])
rownames(rank_mat) <- d$State
print(round(cor(rank_mat, method = "spearman"), 2))
cat("\nEqual-weight TOPSIS vs the dataset's Baseline_Rank (simple average): rho =",
    round(cor(rank_mat[, "Equal weights"], d$Baseline_Rank, method = "spearman"), 3), "\n")

# ---- 5. Save the table and a figure -----------------------------------------
wide <- data.frame(State = d$State, Type = d$Type, Low_Confidence = d$Low_Confidence,
                   Baseline_Rank = d$Baseline_Rank, stringsAsFactors = FALSE)
for (nm in names(rank_results)) {
  rr <- rank_results[[nm]][match(d$State, rank_results[[nm]]$State), ]
  wide[[paste0(nm, " - Closeness")]] <- round(rr$Closeness, 4)
  wide[[paste0(nm, " - Rank")]]      <- rr$Rank
}
wide <- wide[order(wide[["Equal weights - Rank"]]), ]
save_table(wide, "04_topsis_rankings.csv")

top10 <- do.call(rbind, lapply(names(rank_results), function(nm) {
  rr <- rank_results[[nm]]
  rr <- rr[order(rr$Rank), ][1:10, ]
  rr$Persona <- nm
  rr$Label <- ifelse(d$Low_Confidence[match(rr$State, d$State)] == 1,
                     paste0(rr$State, "*"), rr$State)
  rr
}))
top10$Persona <- factor(top10$Persona, levels = names(rank_results))
top10$Key <- reorder(paste(top10$Persona, top10$Label, sep = "___"), top10$Closeness)
p_top <- ggplot(top10, aes(Key, Closeness)) +
  geom_col(fill = "#2E86C1") +
  coord_flip() +
  facet_wrap(~ Persona, scales = "free_y", nrow = 1) +
  scale_x_discrete(labels = function(x) sub("^.*___", "", x)) +
  labs(title = "TOPSIS top 10 for each persona",
       subtitle = "Closeness to the ideal solution (higher = better); * = low data confidence",
       x = NULL, y = "Closeness coefficient") +
  theme_minimal(base_size = 9)
save_figure(p_top, "04_topsis_top10.png", width = 13, height = 4.8)

cat("\n=== TOPSIS complete ===\n")