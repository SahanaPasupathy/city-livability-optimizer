toy <- rbind(P = c(0, 0), Q = c(0.1, 0), R = c(0.5, 0), S = c(1, 1))
colnames(toy) <- c("c1", "c2")

test_that("similar States/UTs are the nearest by Euclidean distance", {
  s <- similar_states(toy, "P", 3)
  expect_equal(s$State, c("Q", "R", "S"))
  expect_equal(s$Distance, c(0.1, 0.5, sqrt(2)))
  expect_equal(s$Similarity_Rank, 1:3)
})

test_that("a State/UT is never listed as similar to itself", {
  expect_false("P" %in% similar_states(toy, "P", 3)$State)
})

test_that("k is capped at the number of other States/UTs", {
  expect_equal(nrow(similar_states(toy, "P", 10)), 3)
})

test_that("similarity is symmetric in distance", {
  d <- distance_matrix(toy)
  expect_equal(d["P", "S"], d["S", "P"])
  expect_equal(similar_states(toy, "S", 1)$Distance, similar_states(toy, "R", 3)$Distance[3])
})

test_that("unknown names are rejected", {
  expect_error(similar_states(toy, "Z"), "Unknown State/UT")
})