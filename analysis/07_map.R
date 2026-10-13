# ==============================================================================
# Step 7.0 - Interactive India map shaded by personalised TOPSIS closeness
# ------------------------------------------------------------------------------
# Purpose : Prove the map works BEFORE it goes into the Shiny app: all 36
#           States/UTs drawn, names match the dataset, colours follow the score,
#           hover cards show rank and the seven criterion scores.
# Input   : data/State_UT_Livability_Dataset_Final.csv,
#           data/map/india_map_paths.csv, R/*.R
# Output  : printed checks + one standalone HTML map per persona in outputs/maps/
# Run from: the project root (open the .Rproj file, then source this script).
# ==============================================================================

source(file.path("R", "common.R"))
source(file.path("R", "ahp.R"))
source(file.path("R", "topsis.R"))
source(file.path("R", "map.R"))

d <- load_livability_data()
X <- criteria_matrix(d)
low_conf <- d$State[d$Low_Confidence == 1]
cat("\n=== STEP 7.0: INTERACTIVE INDIA MAP ===\n")

n_pass <- 0; n_fail <- 0
check <- function(label, ok) {
  ok <- isTRUE(ok)
  if (ok) n_pass <<- n_pass + 1 else n_fail <<- n_fail + 1
  cat(sprintf("[%s] %s\n", ifelse(ok, "PASS", "FAIL"), label))
}

# ---- 1. The outline file -----------------------------------------------------
cat("\n--- 1. Outline file ---\n")
m <- load_map_paths()
cat("States/UTs in the map file:", nrow(m), "\n")
cat("Very small States/UTs drawn with a circle marker:\n  ",
    paste(m$State[m$Marker], collapse = ", "), "\n")
check("Map file has 36 States/UTs", nrow(m) == 36)
check("Map names match the dataset names exactly (no spelling differences)",
      setequal(m$State, d$State))
check("Every State/UT has a drawable outline (path starts with M and ends with Z)",
      all(grepl("^M", m$Path) & grepl("Z$", m$Path)))
check("Exactly 7 very small States/UTs have circle markers", sum(m$Marker) == 7)

# ---- 2. Colours follow the score ---------------------------------------------
cat("\n--- 2. Colours ---\n")
fam <- topsis(X, run_ahp(PERSONAS[["Family"]])$weights)$results
cols <- closeness_colours(fam$Closeness)
brightness <- colSums(grDevices::col2rgb(cols))
best  <- which.min(fam$Rank); worst <- which.max(fam$Rank)
cat(sprintf("Family persona: best = %s (%s), worst = %s (%s)\n",
            fam$State[best], cols[best], fam$State[worst], cols[worst]))
check("Every State/UT gets a valid colour", all(grepl("^#[0-9A-F]{6}$", cols)))
check("The best-ranked State/UT gets the darkest colour",
      brightness[best] == min(brightness))
check("The worst-ranked State/UT gets the lightest colour",
      brightness[worst] == max(brightness))
check("Higher closeness never gets a lighter colour (monotonic)",
      !is.unsorted(rev(brightness[order(fam$Closeness)])))
check("Equal closeness values do not crash the colour scale",
      length(closeness_colours(rep(0.5, 5))) == 5)

# ---- 3. The map itself -------------------------------------------------------
cat("\n--- 3. Building the maps ---\n")
dir.create(file.path("outputs", "maps"), showWarnings = FALSE, recursive = TRUE)
weight_sets <- c(lapply(PERSONAS, function(a) run_ahp(a)$weights),
                 list(`Equal weights` = setNames(rep(1 / 7, 7), CRITERIA)))
files <- character(0)
for (nm in names(weight_sets)) {
  res  <- topsis(X, weight_sets[[nm]])$results
  page <- map_page(render_india_map(res, X, low_conf),
                   title = paste("Livability match map:", nm),
                   note  = "Darker = better match for these priorities. Hover a State/UT for its rank and scores.")
  slug <- tolower(gsub("[^A-Za-z]+", "_", nm))
  f <- file.path("outputs", "maps", paste0("07_map_", slug, ".html"))
  htmltools::save_html(page, f)
  files <- c(files, f)
  cat("Saved map   :", f, "\n")
}

html <- paste(readLines(files[2], warn = FALSE), collapse = "\n")
count <- function(pattern) lengths(regmatches(html, gregexpr(pattern, html, fixed = TRUE)))[1]
check("Map HTML has exactly 43 hoverable shapes (36 outlines + 7 small-State markers)",
      count("data-state=") == 43)
check("Hover card shows rank and all seven criteria (Family, Himachal Pradesh)",
      grepl("Himachal Pradesh", html, fixed = TRUE) &&
        all(sapply(CRITERIA, function(cn) grepl(cn, html, fixed = TRUE))))
check("Exactly three medal badges (top 3) are drawn",
      count("data-badge-state=") == 3)
check("Exactly the 4 low-confidence States/UTs carry the dashed-outline class",
      count("ilm-state ilm-low") == length(low_conf) && length(low_conf) == 4)
check("A mismatched State name is rejected with a clear error",
      inherits(try(render_india_map(transform(fam, State = ifelse(State == "Goa", "Goaa", State)),
                                    X, low_conf), silent = TRUE), "try-error"))
check("All four map files were written", all(file.exists(files)))
cat(sprintf("\nChecks passed: %d   failed: %d\n", n_pass, n_fail))

cat("\nOpen a map: in the Files pane go to outputs/maps, click a .html file and choose\n",
    "'View in Web Browser'. Hover over States/UTs; click one to select it.\n", sep = "")
if (interactive()) utils::browseURL(normalizePath(files[2]))
cat("\n=== Step 7.0 complete ===\n")