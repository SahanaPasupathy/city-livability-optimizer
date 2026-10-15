# Loads the project's functions and data before the tests run.
# testthat runs from tests/testthat/, so the project root is two levels up.
ROOT_DIR <- if (file.exists(file.path("..", "..", "R", "common.R"))) {
  file.path("..", "..")
} else if (file.exists(file.path("R", "common.R"))) {
  "."
} else {
  stop("Cannot find the project root. Open the .Rproj file first.")
}
for (f in c("common", "ahp", "topsis", "similarity", "explain", "compare",
            "app_logic", "sensitivity")) {
  source(file.path(ROOT_DIR, "R", paste0(f, ".R")))
}
DATA <- load_livability_data(file.path(ROOT_DIR, "data", "State_UT_Livability_Dataset_Final.csv"))
XM   <- criteria_matrix(DATA)
LOWC <- DATA$State[DATA$Low_Confidence == 1]