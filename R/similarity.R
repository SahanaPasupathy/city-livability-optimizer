# ==============================================================================
# Similar States/UTs
# ------------------------------------------------------------------------------
# This is NOT the personalised recommendation. TOPSIS answers "which State/UT is
# best FOR YOU" (it uses your AHP weights). Similarity answers "which State/UTs
# have a LIKE profile" and ignores weights completely: it is the plain Euclidean
# distance between two States' seven normalised criterion scores.
# ==============================================================================

#' Find the States/UTs whose seven-score profile is closest to a chosen State/UT.
#'
#' distance(a, b) = sqrt( sum over the 7 criteria of (score_a - score_b)^2 )
#' The chosen State/UT is never returned as its own neighbour.
#'
#' @param X numeric matrix, States/UTs (named rows) x criteria (columns).
#' @param state name of the State/UT to compare against (must be a row name).
#' @param k how many similar States/UTs to return (at most nrow(X) - 1).
#' @return data.frame(State, Distance, Similarity_Rank), nearest first.
similar_states <- function(X, state, k = 3) {
  if (!is.matrix(X) || is.null(rownames(X))) {
    stop("X must be a matrix with State names as row names.", call. = FALSE)
  }
  if (length(state) != 1 || !state %in% rownames(X)) {
    stop("Unknown State/UT: ", paste(state, collapse = ", "), call. = FALSE)
  }
  if (anyNA(X)) stop("X must not contain missing values.", call. = FALSE)
  k <- min(max(as.integer(k), 1L), nrow(X) - 1L)
  
  gaps <- sweep(X, 2, X[state, ], "-")          # score difference on each criterion
  dist <- sqrt(rowSums(gaps^2))                 # Euclidean distance to `state`
  dist <- dist[names(dist) != state]            # drop the State itself
  dist <- sort(dist)[seq_len(k)]                # nearest first
  
  data.frame(State = names(dist), Distance = unname(dist),
             Similarity_Rank = seq_along(dist),
             row.names = NULL, stringsAsFactors = FALSE)
}

#' Full pairwise distance matrix (used only for validation).
#' @param X numeric matrix, States/UTs x criteria.
#' @return symmetric matrix of Euclidean distances.
distance_matrix <- function(X) as.matrix(dist(X, method = "euclidean"))