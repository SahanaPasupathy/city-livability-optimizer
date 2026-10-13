# ==============================================================================
# Step 6 - Personalised rankings versus the equal-weight baseline
# ------------------------------------------------------------------------------
# Purpose : Show that personalisation really changes the answer: how the top 3
#           differs, how many places each State/UT moves, and who moves most.
# Baseline: the same TOPSIS with equal weights (1/7 each), so the only thing
#           that differs is the user's AHP weights.
# Input   : data/State_UT_Livability_Dataset_Final.csv, R/*.R
# Output  : printed comparison + checks, 1 table, 1 figure in outputs/
# Run from: the project root (open the .Rproj file, then source this script).
# ==============================================================================

library(ggplot2)
source(file.path("R", "common.R"))
source(file.path("R", "ahp.R"))
source(file.path("R", "topsis.R"))
source(file.path("R", "compare.R"))

d <- load_livability_data()
X <- criteria_matrix(d)
low_conf <- d$State[d$Low_Confidence == 1]
cat("\n=== STEP 6: PERSONALISED RANKING vs EQUAL-WEIGHT BASELINE ===\n")

n_pass <- 0; n_fail <- 0
check <- function(label, ok) {
  ok <- isTRUE(ok)
  if (ok) n_pass <<- n_pass + 1 else n_fail <<- n_fail + 1
  cat(sprintf("[%s] %s\n", ifelse(ok, "PASS", "FAIL"), label))
}

equal_w  <- setNames(rep(1 / 7, 7), CRITERIA)
baseline <- topsis(X, equal_w)
persona_topsis <- lapply(PERSONAS, function(a) topsis(X, run_ahp(a)$weights))
changes <- lapply(persona_topsis, function(r) rank_change(baseline, r))

# ---- 1. Top 3: baseline versus each persona ---------------------------------
cat("\n--- 1. Top 3 under equal weights versus top 3 for each persona ---\n")
base_top3 <- head(baseline$results[order(baseline$results$Rank), "State"], 3)
cat(sprintf("%-20s %s\n", "Equal weights", paste(base_top3, collapse = " > ")))
for (nm in names(changes)) {
  ch <- changes[[nm]]
  cat(sprintf("%-20s %-48s (%d of the baseline's top 3 kept)\n", nm,
              paste(head(ch$State, 3), collapse = " > "), top_k_overlap(ch, 3)))
}

# ---- 2. Biggest movers -------------------------------------------------------
cat("\n--- 2. Biggest movers (positive = moves UP for this persona) ---\n")
show_movers <- function(df) {
  df$Flag <- ifelse(df$State %in% low_conf, "low confidence", "")
  print(df[, c("State", "Baseline_Rank", "Personal_Rank", "Rank_Change", "Flag")],
        row.names = FALSE)
}
for (nm in names(changes)) {
  mv <- top_movers(changes[[nm]], 5)
  cat("\n", nm, " - climbs the most:\n", sep = ""); show_movers(mv$up)
  cat(nm, " - falls the most:\n", sep = "");        show_movers(mv$down)
}

# ---- 3. Summary of how different the rankings are ---------------------------
cat("\n--- 3. Summary: how much does personalisation change the ranking? ---\n")
summary_tbl <- do.call(rbind, lapply(names(changes), function(nm) {
  ch <- changes[[nm]]
  data.frame(
    Persona            = nm,
    Spearman_vs_Equal  = round(cor(ch$Baseline_Rank, ch$Personal_Rank,
                                   method = "spearman"), 3),
    Top3_Overlap       = top_k_overlap(ch, 3),
    Top10_Overlap      = top_k_overlap(ch, 10),
    Median_Move        = median(abs(ch$Rank_Change)),
    Largest_Rise       = max(ch$Rank_Change),
    Largest_Fall       = min(ch$Rank_Change),
    States_Moving_5Plus = sum(abs(ch$Rank_Change) >= 5),
    stringsAsFactors = FALSE)
}))
print(summary_tbl, row.names = FALSE)
cat("\nHow to read it: Spearman 1 = identical order. Top3/Top10 overlap = how many\n",
    "of the equal-weight top 3 / top 10 are still in the persona's top 3 / top 10.\n",
    "States_Moving_5Plus = number of States/UTs that change by 5 or more places.\n", sep = "")

# ---- 4. Validation checks ----------------------------------------------------
cat("\n--- 4. Validation checks ---\n")
same_as_base <- rank_change(baseline, baseline)
check("Comparing the baseline with itself gives zero movement for all 36",
      all(same_as_base$Rank_Change == 0) && nrow(same_as_base) == 36)
fam <- changes[["Family"]]
check("Rank_Change equals baseline rank minus personalised rank (independent recalculation)",
      all(fam$Rank_Change ==
            baseline$results$Rank[match(fam$State, baseline$results$State)] -
            persona_topsis[["Family"]]$results$Rank[match(fam$State,
                                                          persona_topsis[["Family"]]$results$State)]))
check("Every persona table covers all 36 States/UTs exactly once",
      all(sapply(changes, function(ch) nrow(ch) == 36 && !anyDuplicated(ch$State))))
check("Personal ranks in each table run from 1 to 36 (no ties in this data)",
      all(sapply(changes, function(ch) identical(sort(ch$Personal_Rank), 1:36))))
check("Moves cancel out: total rise equals total fall for each persona",
      all(sapply(changes, function(ch) sum(ch$Rank_Change) == 0)))
mv <- top_movers(changes[["Retiree"]], 5)
check("top_movers(): the biggest climber really has the largest Rank_Change",
      mv$up$Rank_Change[1] == max(changes[["Retiree"]]$Rank_Change) &&
        mv$down$Rank_Change[1] == min(changes[["Retiree"]]$Rank_Change))
check("Personalisation matters: every persona's top 3 differs from the equal-weight top 3",
      all(sapply(changes, function(ch) !setequal(head(ch$State, 3), base_top3))))
check("Personalisation matters: every persona moves at least one State/UT by 10+ places",
      all(sapply(changes, function(ch) max(abs(ch$Rank_Change)) >= 10)))
check("Equal-weight TOPSIS agrees closely with the dataset's own Baseline_Rank (rho > 0.95)",
      cor(baseline$results$Rank[match(d$State, baseline$results$State)],
          d$Baseline_Rank, method = "spearman") > 0.95)
check("An unknown State set gives a clear error",
      inherits(try(rank_change(baseline, topsis(X[-1, ], equal_w)), silent = TRUE),
               "try-error"))
cat(sprintf("\nChecks passed: %d   failed: %d\n", n_pass, n_fail))

# ---- 5. Save the table and a figure -----------------------------------------
long <- do.call(rbind, lapply(names(changes), function(nm) {
  ch <- changes[[nm]]; ch$Persona <- nm; ch
}))
long$Low_Confidence <- long$State %in% low_conf
long <- long[, c("Persona", "State", "Low_Confidence", "Baseline_Rank",
                 "Personal_Rank", "Rank_Change", "Baseline_Closeness",
                 "Personal_Closeness")]
long$Baseline_Closeness <- round(long$Baseline_Closeness, 4)
long$Personal_Closeness <- round(long$Personal_Closeness, 4)
save_table(long, "06_rank_changes.csv")

plot_df <- do.call(rbind, lapply(names(changes), function(nm) {
  mv <- top_movers(changes[[nm]], 5)
  x  <- rbind(mv$up, mv$down)
  x$Persona <- nm
  x$Label <- ifelse(x$State %in% low_conf, paste0(x$State, "*"), x$State)
  x$Direction <- ifelse(x$Rank_Change > 0, "Moves up", "Moves down")
  x
}))
plot_df$Persona <- factor(plot_df$Persona, levels = names(PERSONAS))
plot_df$Key <- reorder(paste(plot_df$Persona, plot_df$Label, sep = "___"),
                       plot_df$Rank_Change)
p_move <- ggplot(plot_df, aes(Key, Rank_Change, fill = Direction)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Persona, scales = "free_y", nrow = 1) +
  scale_x_discrete(labels = function(x) sub("^.*___", "", x)) +
  scale_fill_manual(values = c("Moves up" = "#2E86C1", "Moves down" = "#E67E22")) +
  labs(title = "Biggest rank changes versus equal weights",
       subtitle = "Places gained (+) or lost (-) when the persona's weights replace equal weights; * = low data confidence",
       x = NULL, y = "Change in rank", fill = NULL) +
  theme_minimal(base_size = 9) + theme(legend.position = "bottom")
save_figure(p_move, "06_rank_changes.png", width = 13, height = 5)

cat("\n=== Step 6 complete ===\n")