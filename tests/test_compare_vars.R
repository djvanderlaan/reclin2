library(reclin2)

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
    jw = jaro_winkler()(a,b),
    ja = jaccard()(a,b)
  )
}
tmp <- compare_vars(pairs, "foo", "bar", "bar", testfun)
tmp2 <- 1*(x$bar[pairs$.x] == y$bar[pairs$.y])
stopifnot(isTRUE(all.equal(tmp$foo_jw, tmp2)))
stopifnot(isTRUE(all.equal(tmp$foo_ja, tmp2)))


# Multiple variables are compared resulting in one comparison
# vector
testfun <- function(x, y) {
  cmp <- identical()
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

message("Testing compare_vars() successful")
