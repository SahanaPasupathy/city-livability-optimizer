# Runs all automated tests. From the project root: source("tests/testthat.R")
# (or run source("analysis/11_run_tests.R")).
library(testthat)
test_dir(file.path("tests", "testthat"), reporter = "summary", stop_on_failure = FALSE)