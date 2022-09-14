
expect_equal <- function(x, y) {
  stopifnot(isTRUE(all.equal(x, y)))
}

library(reclin2)

x <- data.table(a = c(1,1,2,2), b = c(1,2,1,2))
y <- data.table(a = c(3,3,2,2), b = c(1,2,1,2))

pairs <- pair_minsim(x, y, on = c("b", "a"), minsim = 2)
expect_equal(names(pairs), c(".x", ".y", "simsum"))
expect_equal(pairs$.x, c(3,4))
expect_equal(pairs$.y, c(3,4))
expect_equal(attr(pairs, "x"), x)
expect_equal(attr(pairs, "y"), y)

pairs <- pair_minsim(x, y, on = c("a", "b"), minsim = 2, 
  keep_simsum = FALSE, add_xy = FALSE)
expect_equal(names(pairs), c(".x", ".y"))
expect_equal(attr(pairs, "x"), NULL)
expect_equal(attr(pairs, "y"), NULL)

pairs <- pair_minsim(x, y, on = c("b", "a"), minsim = 3)
expect_equal(names(pairs), c(".x", ".y", "simsum"))
expect_equal(nrow(pairs), 0)
expect_equal(attr(pairs, "x"), x)
expect_equal(attr(pairs, "y"), y)

pairs <- pair_minsim(x, on = c("b", "a"), minsim = 1,
  deduplication = TRUE)
expect_equal(names(pairs), c(".x", ".y", "simsum"))
expect_equal(pairs$.x, c(1,1,2,3))
expect_equal(pairs$.y, c(2,3,4,4))

# Regression test for issue #9: pairs are not generated when there are missing
# values
x <- data.table(a = c(1,1,2,2), b = c(1,2,1,2))
y <- data.table(a = c(3,3,2,2), b = c(1,2,1,2))
x$a[1] <- NA
pairs <- pair_minsim(x, y, on = c("b", "a"), minsim = 1)
expect_equal(pairs$.x, c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L))
expect_equal(pairs$.y, c(1L, 3L, 2L, 4L, 1L, 3L, 4L, 2L, 3L, 4L))
expect_equal(pairs$simsum, c(1, 1, 1, 1, 1, 2, 1, 1, 1, 2))

