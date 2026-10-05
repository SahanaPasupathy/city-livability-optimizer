# ==============================================================================
# Step 5 - Top-3 recommendation, dynamic explanation, similar States/UTs
# ------------------------------------------------------------------------------
# Purpose : For each demo persona, take the TOPSIS ranking from Step 4, show the
#           top 3 with an automatically generated explanation, and list the
#           States/UTs most similar to each of them.
# Input   : data/State_UT_Livability_Dataset_Final.csv, R/*.R
# Output  : printed recommendations + checks, 1 table, 1 figure in outputs/
# Run from: the project root (open the .Rproj file, then source this script).
# ==============================================================================

library(ggplot2)
source(file.path("R", "common.R"))
source(file.path("R", "ahp.R"))
source(file.path("R", "topsis.R"))
source(file.path("R", "similarity.R"))
source(file.path("R", "explain.R"))

d <- load_livability_data()
X <- criteria_matrix(d)
low_conf <- d$State[d$Low_Confidence == 1]
cat("\n=== STEP 5: RECOMMENDATION, EXPLANATION, SIMILAR STATES ===\n")

n_pass <- 0; n_fail <- 0
check <- function(label, ok) {
  ok <- isTRUE(ok)
  if (ok) n_pass <<- n_pass + 1 else n_fail <<- n_fail + 1
  cat(sprintf("[%s] %s\n", ifelse(ok, "PASS", "FAIL"), label))
}

persona_weights <- lapply(PERSONAS, function(a) run_ahp(a)$weights)
persona_topsis  <- lapply(persona_weights, function(w) topsis(X, w))

# ---- 1. Recommendations, explanations and similar States --------------------
cat("\n--- 1. Top 3 for each persona ---\n")
rows <- list()
for (nm in names(PERSONAS)) {
  res <- persona_topsis[[nm]]
  cat("\n##########  ", toupper(nm), "  ##########\n", sep = "")
  for (st in top_k(res, 3)$State) {
    cat("\n", "#", res$results$Rank[res$results$State == st], " ", st, "\n", sep = "")
    cat(paste0("  - ", explain_state(res, persona_weights[[nm]], st, low_conf)),
        sep = "\n")
    sim <- similar_states(X, st, k = 3)
    cat("  Similar States/UTs (same seven-score profile, ignores your weights): ",
        paste(sprintf("%s (distance %.2f)", sim$State, sim$Distance), collapse = ", "),
        "\n", sep = "")
    rows[[length(rows) + 1]] <- data.frame(
      Persona = nm, Rank = res$results$Rank[res$results$State == st], State = st,
      Closeness = round(res$results$Closeness[res$results$State == st], 4),
      Low_Confidence = st %in% low_conf,
      Similar_1 = sim$State[1], Similar_2 = sim$State[2], Similar_3 = sim$State[3],
      stringsAsFactors = FALSE)
  }
}
recs <- do.call(rbind, rows)

# ---- 2. Similarity is not the same thing as the ranking ---------------------
cat("\n--- 2. Similar States are not simply the next-ranked States ---\n")
for (nm in names(PERSONAS)) {
  res <- persona_topsis[[nm]]
  best <- top_k(res, 1)$State
  nxt  <- top_k(res, 4)$State[2:4]
  sim  <- similar_states(X, best, k = 3)$State
  cat(sprintf("%-18s #1 = %-17s next in ranking: %-45s most similar: %s\n",
              nm, best, paste(nxt, collapse = ", "), paste(sim, collapse = ", ")))
}

# ---- 3. Validation checks ----------------------------------------------------
cat("\n--- 3. Validation checks ---\n")
D <- distance_matrix(X)
check("Distance matrix is 36 x 36, symmetric, zero diagonal",
      all(dim(D) == c(36, 36)) && isSymmetric(unname(D)) && all(diag(D) == 0))
sim_c <- similar_states(X, "Chandigarh", k = 5)
check("A State/UT is never listed as similar to itself",
      all(sapply(rownames(X), function(s) !(s %in% similar_states(X, s, k = 35)$State))))
check("Similar States are sorted from nearest to farthest",
      !is.unsorted(sim_c$Distance))
loop_d <- sapply(rownames(X), function(s) sqrt(sum((X[s, ] - X["Chandigarh", ])^2)))
check("Distances match a loop-based recalculation (Chandigarh)",
      max(abs(sim_c$Distance - loop_d[sim_c$State])) < 1e-12)
check("Nearest State/UT matches the smallest value in the distance matrix",
      sim_c$State[1] == names(which.min(D["Chandigarh", rownames(X) != "Chandigarh"])))
check("Asking for k = 3 returns exactly 3 States/UTs",
      nrow(similar_states(X, "Goa", k = 3)) == 3)
check("An unknown State/UT gives a clear error",
      inherits(try(similar_states(X, "Atlantis"), silent = TRUE), "try-error"))

fam <- persona_topsis[["Family"]]; yp <- persona_topsis[["Young Professional"]]
check("Top-3 table has 3 rows per persona (9 in all)", nrow(recs) == 9)
check("Top 3 are the Rank 1, 2, 3 States of the TOPSIS result",
      all(top_k(fam, 3)$Rank == 1:3))
txt_f <- explain_state(fam, persona_weights[["Family"]], "Chandigarh", low_conf)
txt_y <- explain_state(yp,  persona_weights[["Young Professional"]], "Chandigarh", low_conf)
check("Explanation quotes the exact rank and closeness from TOPSIS",
      grepl(sprintf("%.3f", fam$results$Closeness[fam$results$State == "Chandigarh"]),
            txt_f[1], fixed = TRUE) &&
        grepl(ordinal(fam$results$Rank[fam$results$State == "Chandigarh"]), txt_f[1], fixed = TRUE))
check("Same State/UT, different persona -> different explanation (it is dynamic)",
      !identical(txt_f, txt_y))
check("Low-confidence States/UTs get a caution line; others do not",
      any(grepl("Caution", txt_f)) &&
        !any(grepl("Caution", explain_state(fam, persona_weights[["Family"]], "Himachal Pradesh", low_conf))))
all_text <- unlist(lapply(names(PERSONAS), function(nm) {
  lapply(top_k(persona_topsis[[nm]], 3)$State, function(st)
    explain_state(persona_topsis[[nm]], persona_weights[[nm]], st, low_conf))
}))
check("No explanation sentence contains NA or NaN (all 9 explanations)",
      !any(grepl("\\bNA\\b|NaN", all_text)))
check("ordinal() works: 1st 2nd 3rd 4th 11th 12th 13th 21st",
      identical(ordinal(c(1, 2, 3, 4, 11, 12, 13, 21)),
                c("1st", "2nd", "3rd", "4th", "11th", "12th", "13th", "21st")))
cat(sprintf("\nChecks passed: %d   failed: %d\n", n_pass, n_fail))

# ---- 4. Save the table and a figure -----------------------------------------
save_table(recs, "05_recommendations.csv")

prof <- do.call(rbind, lapply(seq_len(nrow(recs)), function(i) {
  data.frame(Persona = recs$Persona[i],
             State = paste0("#", recs$Rank[i], " ", recs$State[i]),
             Criterion = factor(CRITERIA, levels = CRITERIA),
             Score = X[recs$State[i], ], stringsAsFactors = FALSE)
}))
prof$Persona <- factor(prof$Persona, levels = names(PERSONAS))
p_prof <- ggplot(prof, aes(Criterion, Score, fill = State)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  facet_wrap(~ Persona, ncol = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Criterion scores of each persona's top 3",
       subtitle = "Normalised scores (0 = worst State/UT, 1 = best); the persona's weights decide which bars matter",
       x = NULL, y = "Score", fill = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1), legend.position = "bottom")
save_figure(p_prof, "05_top3_profiles.png", width = 9, height = 9)

cat("\n=== Step 5 complete ===\n")