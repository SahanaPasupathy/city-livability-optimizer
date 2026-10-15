# ==============================================================================
# Step 9 - Sensitivity and robustness
# ------------------------------------------------------------------------------
# Purpose : Test whether the recommendations are stable.
#   A. Random changes to the weights (Dirichlet perturbation, 1000 draws)
#   B. Ignoring one criterion at a time
#   C. Leaving out the 4 low-confidence States/UTs
#   D. A different method: TOPSIS versus a simple weighted sum
#   E. A different normalisation: scores as they are versus vector normalisation
# Input   : data/State_UT_Livability_Dataset_Final.csv, R/*.R
# Output  : printed results + checks, 3 tables and 1 figure in outputs/
# Run from: the project root (open the .Rproj file, then source this script).
# ==============================================================================

library(ggplot2)
source(file.path("R", "common.R"))
source(file.path("R", "ahp.R"))
source(file.path("R", "topsis.R"))
source(file.path("R", "sensitivity.R"))

d <- load_livability_data()
X <- criteria_matrix(d)
low_conf <- d$State[d$Low_Confidence == 1]
cat("\n=== STEP 9: SENSITIVITY AND ROBUSTNESS ===\n")

n_pass <- 0; n_fail <- 0
check <- function(label, ok) {
  ok <- isTRUE(ok)
  if (ok) n_pass <<- n_pass + 1 else n_fail <<- n_fail + 1
  cat(sprintf("[%s] %s\n", ifelse(ok, "PASS", "FAIL"), label))
}
dir.create(file.path("outputs", "tables"), showWarnings = FALSE, recursive = TRUE)

weight_sets <- c(lapply(PERSONAS, function(a) run_ahp(a)$weights),
                 list(`Equal weights` = setNames(rep(1 / 7, 7), CRITERIA)))
base <- lapply(weight_sets, function(w) topsis(X, w)$results)
top3 <- function(res) paste(res$State[order(res$Rank)][1:3], collapse = " > ")
N_DRAWS <- 1000; CONC <- 50

# ---- 0. Tool checks (do the helpers behave?) --------------------------------
cat("\n--- 0. Checks on the helper functions ---\n")
W <- dirichlet_weights(weight_sets$Family, 500, CONC, seed = 7)
check("Random weight vectors each sum to 1 and are all positive",
      all(abs(rowSums(W) - 1) < 1e-12) && all(W > 0))
check("The average random weights are close to the base weights (within 0.01)",
      max(abs(colMeans(W) - weight_sets$Family / sum(weight_sets$Family))) < 0.01)
check("The same seed gives identical draws; a different seed does not",
      identical(W, dirichlet_weights(weight_sets$Family, 500, CONC, seed = 7)) &&
        !identical(W, dirichlet_weights(weight_sets$Family, 500, CONC, seed = 8)))
check("Larger concentration means smaller changes in the weights",
      mean(apply(dirichlet_weights(weight_sets$Family, 500, 500, 1), 2, sd)) <
        mean(apply(W, 2, sd)))
ws <- weighted_sum(X, weight_sets$Retiree)
check("Weighted sum matches a hand calculation for one State/UT",
      abs(ws$Score[ws$State == "Goa"] -
            sum(X["Goa", ] * weight_sets$Retiree[colnames(X)] / sum(weight_sets$Retiree))) < 1e-12)
check("A ranking correlates perfectly with itself (Spearman = 1)",
      abs(spearman_between(base$Family, base$Family) - 1) < 1e-12)
drop_a <- rank_without(X, weight_sets$Family, "Connectivity")
drop_b <- topsis(X[, CRITERIA != "Connectivity"], weight_sets$Family[CRITERIA != "Connectivity"])$results
check("Setting a weight to 0 gives the same ranking as deleting that column",
      identical(drop_a$Rank, drop_b$Rank))
check("An unknown criterion name is rejected",
      inherits(try(rank_without(X, weight_sets$Family, "Wealth"), silent = TRUE), "try-error"))

# ---- A. Random changes to the weights ---------------------------------------
cat(sprintf("\n--- A. Random weight changes (%d draws, concentration %d) ---\n", N_DRAWS, CONC))
cat("Each weight moves by roughly 0.02-0.06 either way. If the top 3 barely\n",
    "changes, the recommendation does not depend on exact weights.\n", sep = "")
stab <- lapply(names(weight_sets), function(nm) rank_stability(X, weight_sets[[nm]], N_DRAWS, CONC, seed = 2026))
names(stab) <- names(weight_sets)
summary_rows <- lapply(names(stab), function(nm) {
  s <- stab[[nm]]
  data.frame(Set = nm, Base_Top3 = top3(base[[nm]]),
             Same_Number1_Pct = s$top1_same, Same_Top3_Pct = s$top3_same,
             Mean_Spearman = s$spearman, stringsAsFactors = FALSE)
})
summary_A <- do.call(rbind, summary_rows)
print(transform(summary_A, Same_Number1_Pct = round(Same_Number1_Pct, 1),
                Same_Top3_Pct = round(Same_Top3_Pct, 1), Mean_Spearman = round(Mean_Spearman, 3)),
      row.names = FALSE)
cat("\nHow often each base top-3 State/UT stays in the top 3 (% of draws):\n")
for (nm in names(stab)) {
  t3 <- stab[[nm]]$table[1:3, c("State", "Base_Rank", "Top3_Pct", "Rank_Low", "Rank_High")]
  cat(nm, ":\n", sep = ""); print(transform(t3, Top3_Pct = round(Top3_Pct, 1)), row.names = FALSE)
}
tab_A <- do.call(rbind, lapply(names(stab), function(nm) cbind(Set = nm, stab[[nm]]$table)))
write.csv(tab_A, file.path("outputs", "tables", "10_weight_perturbation.csv"), row.names = FALSE)
check("Stability table has 36 States/UTs for each of the 4 weight sets", nrow(tab_A) == 4 * 36)
check("Top-3 percentages lie between 0 and 100 and the average is 300 (three places)",
      all(tab_A$Top3_Pct >= 0 & tab_A$Top3_Pct <= 100) &&
        all(abs(tapply(tab_A$Top3_Pct, tab_A$Set, sum) - 300) < 1e-9))

# ---- B. Ignoring one criterion at a time ------------------------------------
cat("\n--- B. Ignore one criterion at a time ---\n")
rows_B <- list()
for (nm in names(weight_sets)) for (cn in CRITERIA) {
  r <- rank_without(X, weight_sets[[nm]], cn)
  rows_B[[length(rows_B) + 1]] <- data.frame(
    Set = nm, Ignored = cn, Top3 = top3(r),
    Spearman_vs_base = spearman_between(base[[nm]], r),
    Top3_shared = topk_shared(base[[nm]], r, 3), stringsAsFactors = FALSE)
}
tab_B <- do.call(rbind, rows_B)
write.csv(tab_B, file.path("outputs", "tables", "10_criterion_removal.csv"), row.names = FALSE)
for (nm in names(weight_sets)) {
  cat("\n", nm, " (base top 3: ", top3(base[[nm]]), ")\n", sep = "")
  print(transform(tab_B[tab_B$Set == nm, -1], Spearman_vs_base = round(Spearman_vs_base, 3)), row.names = FALSE)
}
check("Ignoring a criterion changes the ranking only slightly on average (mean Spearman > 0.80)",
      mean(tab_B$Spearman_vs_base) > 0.80)

# ---- C. Leaving out low-confidence States/UTs --------------------------------
cat("\n--- C. Leave out the 4 low-confidence States/UTs ---\n")
keep <- !(rownames(X) %in% low_conf)
rows_C <- lapply(names(weight_sets), function(nm) {
  r <- topsis(X[keep, , drop = FALSE], weight_sets[[nm]])$results
  common <- base[[nm]][base[[nm]]$State %in% r$State, ]
  common$Rank <- rank(common$Rank, ties.method = "min")
  data.frame(Set = nm, Top3_all_36 = top3(base[[nm]]), Top3_without_low_conf = top3(r),
             Spearman_on_32 = spearman_between(common, r), stringsAsFactors = FALSE)
})
tab_C <- do.call(rbind, rows_C)
print(transform(tab_C, Spearman_on_32 = round(Spearman_on_32, 3)), row.names = FALSE)
check("Excluded ranking has 32 States/UTs", nrow(topsis(X[keep, ], weight_sets$Family)$results) == 32)

# ---- D and E. Different method / normalisation ---------------------------------
cat("\n--- D. TOPSIS versus a weighted sum, and E. scores as they are versus vector normalisation ---\n")
rows_DE <- lapply(names(weight_sets), function(nm) {
  w <- weight_sets[[nm]]
  ws <- weighted_sum(X, w); vn <- topsis(X, w, normalise = "vector")$results
  data.frame(Set = nm,
             WS_top3 = top3(ws), WS_Spearman = spearman_between(base[[nm]], ws),
             WS_top3_shared = topk_shared(base[[nm]], ws, 3),
             Vec_top3 = top3(vn), Vec_Spearman = spearman_between(base[[nm]], vn),
             Vec_top3_shared = topk_shared(base[[nm]], vn, 3), stringsAsFactors = FALSE)
})
tab_DE <- do.call(rbind, rows_DE)
print(transform(tab_DE, WS_Spearman = round(WS_Spearman, 3), Vec_Spearman = round(Vec_Spearman, 3)),
      row.names = FALSE)
write.csv(rbind(
  data.frame(Test = "Low-confidence left out", tab_C[, c("Set", "Top3_all_36", "Top3_without_low_conf", "Spearman_on_32")] |>
               setNames(c("Set", "Base_Top3", "Alt_Top3", "Spearman"))),
  data.frame(Test = "Weighted sum", Set = tab_DE$Set, Base_Top3 = sapply(tab_DE$Set, function(s) top3(base[[s]])),
             Alt_Top3 = tab_DE$WS_top3, Spearman = tab_DE$WS_Spearman),
  data.frame(Test = "Vector normalisation", Set = tab_DE$Set, Base_Top3 = sapply(tab_DE$Set, function(s) top3(base[[s]])),
             Alt_Top3 = tab_DE$Vec_top3, Spearman = tab_DE$Vec_Spearman)),
  file.path("outputs", "tables", "10_method_comparison.csv"), row.names = FALSE)
check("TOPSIS and the weighted sum agree strongly in every case (Spearman > 0.90)",
      all(tab_DE$WS_Spearman > 0.90))

# ---- Figure: how often each base top-3 State/UT stays in the top 3 ----------
fig <- do.call(rbind, lapply(names(stab), function(nm) {
  t3 <- stab[[nm]]$table[1:3, ]; data.frame(Set = nm, State = t3$State, Top3_Pct = t3$Top3_Pct)
}))
fig$Set <- factor(fig$Set, levels = names(weight_sets))
p <- ggplot(fig, aes(x = reorder(State, Top3_Pct), y = Top3_Pct)) +
  geom_col(fill = "#4B3FA8", width = 0.6) +
  geom_text(aes(label = sprintf("%.0f%%", Top3_Pct)), hjust = -0.1, size = 3.3) +
  coord_flip(ylim = c(0, 110)) + facet_wrap(~Set, scales = "free_y") +
  labs(title = "How often each recommended State/UT stays in the top 3",
       subtitle = sprintf("%d random weight changes per persona (each weight moves by about 0.02-0.06)", N_DRAWS),
       x = NULL, y = "% of random weight sets in which it is in the top 3") +
  theme_minimal(base_size = 11)
save_figure(p, "10_top3_stability.png", width = 11, height = 6)

cat(sprintf("\nChecks passed: %d   failed: %d\n", n_pass, n_fail))
cat("\nTables: outputs/tables/10_*.csv   Figure: outputs/figures/10_top3_stability.png\n")
cat("\n=== Step 9 complete ===\n")