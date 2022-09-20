expect_equal <- function(x, y) {
  stopifnot(isTRUE(all.equal(x, y)))
}

library(reclin2)

set.seed(2)
d1 <- reclin2:::distribute_over_cluster(100, 4)
a1 <- runif(5)
d2 <- reclin2:::distribute_over_cluster(100, 4)
a2 <- runif(5)

set.seed(2)
a1_a2 <- runif(10)

expect_equal(c(a1, a2), a1_a2)
expect_equal(d1, d2)

