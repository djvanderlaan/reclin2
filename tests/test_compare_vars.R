library(reclin2)
source("helpers.R")

message("Testing compare_vars()")

x <- data.table(foo = 1:3, bar = c("aa", "bb", "cc"))
y <- data.table(foo = 3:4, bar = c("cc", "dd"))
pairs <- pair(x, y)

tmp <- compare_vars(pairs, "foo")
tmp2 <- x$foo[pairs$.x] == y$foo[pairs$.y]
stopifnot(is.logical(tmp$foo))
stopifnot(isTRUE(all.equal(as.logical(tmp$foo), tmp2)))

tmp <- copy(pairs)
compare_vars(tmp, "foo", inplace = TRUE)
tmp2 <- x$foo[pairs$.x] == y$foo[pairs$.y]
stopifnot(is.logical(tmp$foo))
stopifnot(isTRUE(all.equal(as.logical(tmp$foo), tmp2)))

tmp <- compare_vars(pairs, "fooc", "foo")
tmp2 <- x$foo[pairs$.x] == y$foo[pairs$.y]
stopifnot(is.logical(tmp$fooc))
stopifnot(isTRUE(all.equal(as.logical(tmp$fooc), tmp2)))

tmp <- compare_vars(pairs, "fooc", "foo", "foo")
tmp2 <- x$foo[pairs$.x] == y$foo[pairs$.y]
stopifnot(is.logical(tmp$fooc))
stopifnot(isTRUE(all.equal(as.logical(tmp$fooc), tmp2)))


# Comparing on one variable generates two resulting comparison
# vectors
testfun <- function(a, b) {
  data.table(
    jw = cmp_jarowinkler()(a,b),
    ja = cmp_jaccard()(a,b)
  )
}
tmp <- compare_vars(pairs, "foo", "bar", "bar", testfun)
tmp2 <- 1*(x$bar[pairs$.x] == y$bar[pairs$.y])
stopifnot(isTRUE(all.equal(tmp$foo_jw, tmp2)))
stopifnot(isTRUE(all.equal(tmp$foo_ja, tmp2)))


# Multiple variables are compared resulting in one comparison
# vector
testfun <- function(x, y) {
  cmp <- cmp_identical()
  c1 <- cmp(x[[1]], y[[1]]) + cmp(x[[2]], y[[2]])
  c2 <- cmp(x[[1]], y[[2]]) + cmp(x[[2]], y[[1]])
  pmax(c1, c2)
}
tmp <- compare_vars(pairs, "foo", c("foo", "bar"), comparator = testfun)
tmp2 <- (x$foo[pairs$.x] == y$foo[pairs$.y]) + 
  (x$bar[pairs$.x] == y$bar[pairs$.y]) 
stopifnot(is.numeric(tmp$foo))
stopifnot(isTRUE(all.equal(as.numeric(tmp$foo), tmp2)))

# Pairs has zero records
x <- data.table(foo = 1:3, bar = c("aa", "bb", "cc"))
y <- data.table(foo = 3:4, bar = c("cc", "dd"))
y <- y[FALSE,]
pairs <- pair(x, y)
tmp <- compare_vars(pairs, "foo")
tmp2 <- x$foo[pairs$.x] == y$foo[pairs$.y]
stopifnot(is.logical(tmp$foo))
stopifnot(isTRUE(all.equal(as.logical(tmp$foo), tmp2)))

# Passing additional arguments on to the comparison function
x <- data.table(foo = 1:3, bar = c("aa", "bb", "cc"))
y <- data.table(foo = 3:4, bar = c("cc", "dd"))
pairs <- pair(x, y)

testfun <- function(x, y, foo = 1) {
  rep(foo, length(x))
}
tmp <- compare_vars(pairs, "foo", comparator = testfun)
expect_equal(tmp$foo, rep(1, 6), attributes = FALSE)
tmp <- compare_vars(pairs, "foo", comparator = testfun, foo = 5)
expect_equal(tmp$foo, rep(5, 6), attributes = FALSE)

message("Testing compare_vars() successful")


