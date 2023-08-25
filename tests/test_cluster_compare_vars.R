library(reclin2)
source("helpers.R")

message("Testing cluster_compare_vars()")

# Passing additional arguments on to the comparison function
x <- data.table(foo = 1:3, bar = c("aa", "bb", "cc"))
y <- data.table(foo = 3:4, bar = c("cc", "dd"))
cl <- parallel::makeCluster(1)
pairs <- cluster_pair(cl, x, y)

testfun <- function(x, y, foo = 1) {
  rep(foo, length(x))
}
tmp <- compare_vars(pairs, "foo", comparator = testfun)
tmp <- cluster_collect(tmp)
expect_equal(tmp$foo, rep(1, 6), attributes = FALSE)

tmp <- compare_vars(pairs, "foo", comparator = testfun, foo = 5)
tmp <- cluster_collect(tmp)
expect_equal(tmp$foo, rep(5, 6), attributes = FALSE)

message("Testing cluster_compare_vars() successful")

parallel::stopCluster(cl)
