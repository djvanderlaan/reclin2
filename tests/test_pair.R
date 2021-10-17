
expect_equal <- function(x, y) {
  stopifnot(isTRUE(all.equal(x, y)))
}

library(reclin2)

x <- data.table(a = c(1,1,2,2), b = c(1,2,1,2))
y <- data.table(a = c(3,3,2,2), b = c(1,2,1,2))

pairs <- pair(x, y)
expect_equal(names(pairs), c(".x", ".y"))
expect_equal(pairs$.x, rep(1:4, each = 4))
expect_equal(pairs$.y, rep(1:4, times = 4))
expect_equal(attr(pairs, "x"), x)
expect_equal(attr(pairs, "y"), y)

pairs <- pair(x, y, add_xy = FALSE)
expect_equal(attr(pairs, "x"), NULL)
expect_equal(attr(pairs, "y"), NULL)


pairs <- pair(x, deduplication = TRUE)
expect_equal(names(pairs), c(".x", ".y"))
expect_equal(pairs$.x, c(1,1,1,2,2,3))
expect_equal(pairs$.y, c(2,3,4,3,4,4))

