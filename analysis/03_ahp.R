# ==============================================================================
# Step 3 - AHP: pairwise matrix, weights and consistency ratio
# ------------------------------------------------------------------------------
# Purpose : Show and validate the AHP stage for the three demo personas, and
#           prove the 1-5 questionnaire can never produce an inconsistent matrix.
# Input   : none (uses the personas defined in R/ahp.R)
# Output  : printed validation report, 2 tables, 1 figure in outputs/
# Run from: the project root (open the .Rproj file, then source this script).
# ==============================================================================

library(ggplot2)
source(file.path("R", "common.R"))
source(file.path("R", "ahp.R"))

cat("\n=== STEP 3: AHP ===\n")

# ---- 1. Textbook sanity check: a 2 x 2 case with a known answer -------------
cat("\n--- 1. Sanity check on a case we can solve by hand ---\n")
# "A is 3 times as important as B" => weights must be 0.75 and 0.25.
tiny <- matrix(c(1, 3, 1 / 3, 1), nrow = 2, byrow = TRUE,
               dimnames = list(c("A", "B"), c("A", "B")))
tiny_res <- ahp_weights(tiny)
print(round(tiny_res$weights, 4))
cat("Expected 0.75 and 0.25:",
    ifelse(all(abs(tiny_res$weights - c(0.75, 0.25)) < 1e-9), "PASS", "FAIL"), "\n")

# ---- 2. Worked example: the Family persona, step by step --------------------
cat("\n--- 2. Worked example: Family persona ---\n")
fam <- run_ahp(PERSONAS[["Family"]])
cat("Questionnaire answers (1 = not important ... 5 = extremely important):\n")
print(fam$answers)
cat("\n7 x 7 pairwise comparison matrix (row i vs column j):\n")
print(round(fam$matrix, 3))
cat("\nAHP weights (principal eigenvector, sum = 1):\n")
print(round(fam$weights, 4))
cat("\nSum of weights:", round(sum(fam$weights), 6), "\n")
cat(sprintf("lambda_max = %.4f   (n = %d)\n", fam$lambda_max, length(CRITERIA)))
cat(sprintf("CI = (lambda_max - n) / (n - 1) = (%.4f - %d) / %d = %.4f\n",
            fam$lambda_max, length(CRITERIA), length(CRITERIA) - 1, fam$CI))
cat(sprintf("RI (n = 7) = %.2f\n", fam$RI))
cat(sprintf("CR = CI / RI = %.4f / %.2f = %.4f   -> %s (must be <= 0.10)\n",
            fam$CI, fam$RI, fam$CR, ifelse(fam$consistent, "CONSISTENT", "INCONSISTENT")))

# ---- 3. All personas: validation checks -------------------------------------
cat("\n--- 3. Validation checks for each persona ---\n")
persona_results <- lapply(PERSONAS, run_ahp)
for (p in names(persona_results)) {
  v <- validate_ahp(persona_results[[p]])
  cat(sprintf("\n%s: %d of %d checks passed\n", p, sum(v$Passed), nrow(v)))
  failed <- v$Check[!v$Passed]
  if (length(failed) > 0) cat("  FAILED:", paste(failed, collapse = "; "), "\n")
}

# ---- 4. Persona weights and consistency -------------------------------------
cat("\n--- 4. Persona weights ---\n")
weights_tbl <- data.frame(
  Criterion = CRITERIA,
  sapply(persona_results, function(r) r$weights),
  row.names = NULL, check.names = FALSE
)
print(weights_tbl, digits = 3, row.names = FALSE)
cat("\nHighest-weighted criteria per persona:\n")
for (p in names(persona_results)) {
  w <- sort(persona_results[[p]]$weights, decreasing = TRUE)
  cat(sprintf("  %-20s %s\n", p,
              paste0(names(w)[1:3], " (", round(w[1:3], 2), ")", collapse = ", ")))
}
consistency_tbl <- data.frame(
  Persona    = names(persona_results),
  Lambda_max = sapply(persona_results, function(r) r$lambda_max),
  CI         = sapply(persona_results, function(r) r$CI),
  CR         = sapply(persona_results, function(r) r$CR),
  Consistent = sapply(persona_results, function(r) r$consistent),
  row.names = NULL
)
cat("\nConsistency of each persona's matrix:\n")
print(consistency_tbl, digits = 3, row.names = FALSE)
save_table(weights_tbl, "03_persona_weights.csv")
save_table(consistency_tbl, "03_persona_consistency.csv")

# ---- 5. Can ANY set of answers be inconsistent? -----------------------------
cat("\n--- 5. Exhaustive consistency test over every possible questionnaire ---\n")
# There are 5^7 = 78,125 ways to answer. Re-ordering the criteria only permutes
# the matrix, which does not change lambda_max, so it is enough to test each
# multiset of answers once (330 patterns).
grid <- as.matrix(expand.grid(rep(list(1:5), length(CRITERIA))))
patterns <- grid[apply(grid, 1, function(r) all(diff(r) >= 0)), , drop = FALSE]
all_cr <- apply(patterns, 1, function(a) {
  ahp_weights(build_pairwise_matrix(setNames(a, CRITERIA)))$CR
})
cat("Distinct answer patterns tested:", nrow(patterns), "\n")
cat("Largest CR found:", round(max(all_cr), 4),
    " for answers", paste(patterns[which.max(all_cr), ], collapse = " "), "\n")
cat("Patterns with CR > 0.10:", sum(all_cr > 0.10), "\n")
cat("Result:", ifelse(all(all_cr <= 0.10), "PASS - the rule never gives CR > 0.10",
                      "FAIL - some answers are inconsistent"), "\n")

# ---- 6. Figure --------------------------------------------------------------
weights_long <- data.frame(
  Criterion = factor(rep(CRITERIA, times = length(PERSONAS)), levels = CRITERIA),
  Persona   = rep(names(PERSONAS), each = length(CRITERIA)),
  Weight    = unlist(lapply(persona_results, function(r) unname(r$weights)))
)
p_weights <- ggplot(weights_long, aes(Criterion, Weight, fill = Persona)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = c("Young Professional" = "#2E86C1",
                               "Family" = "#28B463", "Retiree" = "#E67E22")) +
  labs(title = "AHP criterion weights for the three demo personas",
       subtitle = "Weights come from the principal eigenvector; each persona sums to 1",
       x = NULL, y = "AHP weight", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "bottom")
save_figure(p_weights, "03_persona_weights.png", width = 8, height = 5)

cat("\n=== AHP complete ===\n")