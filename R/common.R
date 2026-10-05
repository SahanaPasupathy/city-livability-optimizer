# ==============================================================================
# Shared constants and helpers used by every analysis script and the Shiny app.
# Keeping them in ONE place means a calculation is never written twice.
# ==============================================================================

DATA_PATH <- file.path("data", "State_UT_Livability_Dataset_Final.csv")

# The seven livability criteria (same order everywhere in the project).
CRITERIA  <- c("Education", "Safety", "Health", "Housing",
               "Economy", "Environment", "Connectivity")
NORM_COLS <- paste0(CRITERIA, "_norm")

# The 17 raw indicators that feed the criteria.
INDICATORS <- c(
  "Education_Score", "Grade8_Proficiency_Pct", "HigherSec_GER_Pct",
  "Crime_Rate", "Murders_per_Lakh",
  "Immunisation_Pct", "Institutional_Deliveries_Pct", "Health_Worker_Density",
  "Kachha_Houses_Pct", "Clean_Cooking_Fuel_Pct", "Rural_Piped_Water_Pct",
  "Unemployment_Rate_Pct", "MPI_Headcount_Pct", "Avg_Inflation_PriceStability",
  "AirQuality_PM25",
  "Villages_3G4G_Pct", "EVs_per_Lakh"
)

#' Read the livability dataset (never modifies the file).
#' @param path location of the CSV; defaults to the project's data folder.
#' @return data.frame with one row per State/UT.
load_livability_data <- function(path = DATA_PATH) {
  if (!file.exists(path)) {
    stop("Dataset not found at '", path, "'. Open the RStudio project (.Rproj) ",
         "so the working directory is the project root.", call. = FALSE)
  }
  d <- read.csv(path, na.strings = "NA", stringsAsFactors = FALSE)
  needed <- c("State", "Type", "Low_Confidence", NORM_COLS,
              "Baseline_Score_EqualWeights", "Baseline_Rank")
  absent <- setdiff(needed, names(d))
  if (length(absent) > 0) {
    stop("Dataset is missing columns: ", paste(absent, collapse = ", "),
         call. = FALSE)
  }
  d
}

#' The decision matrix: States/UTs (rows) x seven criterion scores (columns).
#' @param d data.frame returned by load_livability_data().
#' @return numeric matrix with State names as row names, CRITERIA as columns.
criteria_matrix <- function(d) {
  m <- as.matrix(d[NORM_COLS])
  rownames(m) <- d$State
  colnames(m) <- CRITERIA
  m
}

#' Save a ggplot into outputs/figures (folder is created if needed).
save_figure <- function(plot, filename, width = 8, height = 5) {
  dir.create(file.path("outputs", "figures"), showWarnings = FALSE,
             recursive = TRUE)
  ggplot2::ggsave(file.path("outputs", "figures", filename), plot,
                  width = width, height = height, dpi = 150)
  cat("Saved figure: outputs/figures/", filename, "\n", sep = "")
}

#' Save a data.frame into outputs/tables (folder is created if needed).
save_table <- function(df, filename) {
  dir.create(file.path("outputs", "tables"), showWarnings = FALSE,
             recursive = TRUE)
  write.csv(df, file.path("outputs", "tables", filename), row.names = FALSE)
  cat("Saved table : outputs/tables/", filename, "\n", sep = "")
}