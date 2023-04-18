
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

stopCluster(cl)

stop("IMPLEMENT ON_BLOCKING")
