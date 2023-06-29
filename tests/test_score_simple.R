
library(reclin2)
library(parallel)
source("helpers.R")

pairs <- data.table(
    .x = c(1L, 1L, 2L, 3L),
    .y = c(1L, 2L, 1L, 2L),
    a  = c(1.0, 0.0, 1.0, 0.0),
    b  = c(0.5, 1.0, 0.0, 1.0),
    c  = c(0.0, NA,  0.5, NA)
  )
class(pairs) <- c("pairs", "data.table")

# === Basic functionality
tmp <- score_simple(pairs, "score", on = c("a", "b", "c"))
expect_equal(names(tmp), c(names(pairs), "score"))
expect_equal(tmp$score, c(1.5, 1.0, 1.5, 1.0))

tmp <- score_simple(pairs, "score", on = c("c", "b"))
expect_equal(names(tmp), c(names(pairs), "score"))
expect_equal(tmp$score, c(0.5, 1.0, 0.5, 1.0))

# === Specifying weights
tmp <- score_simple(pairs, "score", on = c("a", "b", "c"), 
  w1 = c(2, 1, 0.5))
expect_equal(tmp$score, c(2.5, 1.0, 2.25, 1.0))

tmp <- score_simple(pairs, "score", on = c("a", "b", "c"), 
  w1 = c(b = 1, c = 0.5, a = 2.0))
expect_equal(tmp$score, c(2.5, 1.0, 2.25, 1.0))

tmp <- score_simple(pairs, "score", on = c("a", "b", "c"), 
  w1 = list(b = 1, c = 0.5, a = 2.0))
expect_equal(tmp$score, c(2.5, 1.0, 2.25, 1.0))

tmp <- score_simple(pairs, "score", on = c("a", "b", "c"), 
  w1 = list(b = 1, c = 0.5, a = 2.0),
  w0 = c(-1, -2, -3))
expect_equal(tmp$score, c(-1.5, 0.0, -1.25, 0.0))

tmp <- score_simple(pairs, "score", on = c("a", "b", "c"), 
  w1 = list(b = 1, c = 0.5, a = 2.0),
  w0 = c(-1, -2, -3),
  wna = c(b = -1, a = 0, c = -1))
expect_equal(tmp$score, c(-1.5, -1.0, -1.25, -1.0))

tmp <- score_simple(pairs, "score", on = c("a", "b", "c"),
  wna = NA)
expect_equal(tmp$score, c(1.5, NA, 1.5, NA))

# === Edge cases
tmp <- score_simple(pairs, "score", on = character(0))
expect_equal(tmp$score, c(0.0, 0.0, 0.0, 0.0))

tmp <- score_simple(pairs[FALSE,], "score", on = c("a", "b", "c"))
expect_equal(tmp$score, numeric(0))

# === IN PLACE
tmp <- copy(pairs)
score_simple(tmp, "score", on = c("a", "b", "c"), 
  w1 = list(b = 1, c = 0.5, a = 2.0),
  w0 = c(-1, -2, -3),
  wna = c(b = -1, a = 0, c = -1), inplace = TRUE)
expect_equal(tmp$score, c(-1.5, -1.0, -1.25, -1.0))

tmp <- copy(pairs)
score_simple(tmp, "score", on = character(0), inplace = TRUE)
expect_equal(tmp$score, c(0.0, 0.0, 0.0, 0.0))

# === CLUSTER
x <- data.table(a = c(1,1,2,2), b = c(1,2,1,2))
y <- data.table(a = c(3,3,2,2), b = c(1,2,1,2))

cl <- makeCluster(2)

set.seed(103)
pairs <- cluster_pair_blocking(cl, x, y, on = "a")
compare_pairs(pairs, c("a", "b"))

score_simple(pairs, on = c("a", "b"), variable = "score")
tmp <- cluster_collect(pairs)
expect_equal(tmp$score, c(1,2,2,1))

score_simple(pairs, on = c("a", "b"), variable = "score", 
  w1 = c(4,1), w0 = c(-1, -2))
tmp <- cluster_collect(pairs)
expect_equal(tmp$score, c(2, 5, 5, 2))

