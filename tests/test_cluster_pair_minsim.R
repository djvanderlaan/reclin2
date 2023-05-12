
source("helpers.R")

expect_equal_pairs <- function(x, y) {
  setkey(pairs_ref, .x, .y)
  setkey(pairs_loc, .x, .y)
  expect_equal(pairs_ref$.x, pairs_loc$.x)
  expect_equal(pairs_ref$.y, pairs_loc$.y)
}

library(reclin2)
library(parallel)

x <- data.table(a = c(1,1,2,2), b = c(1,2,1,2))
y <- data.table(a = c(3,3,2,2), b = c(1,2,1,2))


cl <- makeCluster(2)

set.seed(103)
pairs <- cluster_pair_minsim(cl, x, y, on = c("a", "b"), minsim = 2)
pairs_ref <- pair_minsim(x, y, on = c("a", "b"), minsim = 2)
pairs_loc <- cluster_collect(pairs)
pairs_ref <- compare_pairs(pairs_ref, c("a", "b"))
pairs_loc <- compare_pairs(pairs_loc, c("a", "b"))
expect_equal_pairs(pairs_ref, pairs_loc)
# The compare_pairs was added to fix a bug where the x after 
# cluster_colled was not longer in the correct order; issue #2

set.seed(103)
pairs <- cluster_pair_minsim(cl, x, on = c("a", "b"), minsim = 1, deduplication = TRUE)
pairs_ref <- pair_minsim(x, on = c("a", "b"), minsim = 1, deduplication = TRUE)
pairs_loc <- cluster_collect(pairs)
pairs_ref <- compare_pairs(pairs_ref, c("a", "b"))
pairs_loc <- compare_pairs(pairs_loc, c("a", "b"))
expect_equal_pairs(pairs_ref, pairs_loc)


# ===== ON_BLOCKING

x <- data.table(a = c(1,1,2,2), b = c(1,2,1,2))
y <- data.table(a = c(3,3,2,2), b = c(1,2,1,2))

set.seed(103)
pairs <- cluster_pair_minsim(cl, x, y, on = "a", on_blocking = "b")
pairs_ref <- pair_minsim(x, y, on = "a", on_blocking = "b")
pairs_loc <- cluster_collect(pairs)
expect_equal_pairs(pairs_ref, pairs_loc)

# Unsing non-existent columns
expect_error(cluster_pair_minsim(cl, x, y, on = "a", on_blocking = "foo"))
expect_error(cluster_pair_minsim(cl, x, y, on = c("a", "foo"), on_blocking = "b"))

set.seed(103)
pairs <- cluster_pair_minsim(cl, x, y, on = "a", on_blocking = "b", minsim = 1)
pairs_ref <- pair_minsim(x, y, on = "a", on_blocking = "b", minsim = 1)
pairs_loc <- cluster_collect(pairs)
expect_equal_pairs(pairs_ref, pairs_loc)

set.seed(103)
pairs <- cluster_pair_minsim(cl, x, y, on = "a", on_blocking = "b", minsim = 2)
pairs_ref <- pair_minsim(x, y, on = "a", on_blocking = "b", minsim = 2)
pairs_loc <- cluster_collect(pairs)
expect_equal_pairs(pairs_ref, pairs_loc)

# Missing values
x <- data.table(a = c(1,NA,2,2), b = c(1,2,1,2))
y <- data.table(a = c(3,3,2,2), b = c(1,2,NA,2))
set.seed(103)
pairs <- cluster_pair_minsim(cl, x, y, on = "a", on_blocking = "b", minsim = 1)
pairs_ref <- pair_minsim(x, y, on = "a", on_blocking = "b", minsim = 1)
pairs_loc <- cluster_collect(pairs)
expect_equal_pairs(pairs_ref, pairs_loc)


# ======== FINISH
stopCluster(cl)
