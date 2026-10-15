# ==============================================================================
# Step 10 - Automated tests (testthat)
# Run from the project root: source("analysis/11_run_tests.R")
# Needs the testthat package: install.packages("testthat")
# ==============================================================================
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Please run install.packages(\"testthat\") first.", call. = FALSE)
}
cat("\n=== STEP 10: AUTOMATED TESTS ===\n\n")
res <- testthat::test_dir(file.path("tests", "testthat"), reporter = "summary",
                          stop_on_failure = FALSE)
df <- as.data.frame(res)
cat(sprintf("\nTests run: %d   expectations passed: %d   failed: %d   errors: %d\n",
            nrow(df), sum(df$passed), sum(df$failed), sum(df$error)))
if (sum(df$failed) + sum(df$error) == 0) cat("ALL TESTS PASSED\n") else cat("SOME TESTS FAILED - see above\n")
cat("\n=== Step 10 complete ===\n")