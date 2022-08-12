
expect_equal <- function(x, y) {
  stopifnot(isTRUE(all.equal(x, y)))
}

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
pairs <- cluster_pair_blocking(cl, x, y, on = "a")
pairs_ref <- pair_blocking(x, y, on = "a")
pairs_loc <- cluster_collect(pairs)
pairs_ref <- compare_pairs(pairs_ref, c("a", "b"))
pairs_loc <- compare_pairs(pairs_loc, c("a", "b"))
expect_equal_pairs(pairs_ref, pairs_loc)
# The compare_pairs was added to fix a bug where the x after 
# cluster_colled was not longer in the correct order; issue #2

set.seed(103)
pairs <- cluster_pair_blocking(cl, x, on = "a", deduplication = TRUE)
pairs_ref <- pair_blocking(x, on = "a", deduplication = TRUE)
pairs_loc <- cluster_collect(pairs)
pairs_ref <- compare_pairs(pairs_ref, c("a", "b"))
pairs_loc <- compare_pairs(pairs_loc, c("a", "b"))
expect_equal_pairs(pairs_ref, pairs_loc)

stopCluster(cl)

