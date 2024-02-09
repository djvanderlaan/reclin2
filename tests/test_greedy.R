
library(reclin2)
source("helpers.R")

message("Testing greedy()")


dta <- read.csv2(textConnection('x;y;w
1;1;10
1;2;9
4;1;8
4;3;7
5;3;7
5;4;7
6;3;6'
))

res <- greedy(dta$x, dta$y, dta$w)
expect_equal(res, c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))

# Change order; greedy should sort
set.seed(10)
o <- sample(nrow(dta))
res <- greedy(dta$x[o], dta$y[o], dta$w[o])
expect_equal(res, c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)[o])

# Empty input
res <- greedy(dta$x[FALSE], dta$y[FALSE], dta$w[FALSE])
expect_equal(res, logical(0))

# Missing values
dta2 <- dta
dta2$w[1] <- NA
expect_error(
  res <- greedy(dta2$x, dta2$y, dta2$w)
)

# include_ties = TRUE
res <- greedy(dta$x, dta$y, dta$w, include_ties = TRUE)
expect_equal(res, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))

# include_ties = TRUE
# Change order; greedy should sort
set.seed(10)
o <- sample(nrow(dta))
res <- greedy(dta$x[o], dta$y[o], dta$w[o], include_ties = TRUE)
expect_equal(res, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)[o])

# include_ties = TRUE
# Empty input
res <- greedy(dta$x[FALSE], dta$y[FALSE], dta$w[FALSE], include_ties = TRUE)
expect_equal(res, logical(0))

# include_ties = TRUE
# Missing values
dta2 <- dta
dta2$w[1] <- NA
expect_error(
  res <- greedy(dta2$x, dta2$y, dta2$w, include_ties = TRUE)
)

# n, m

dta <- read.csv2(textConnection('x;y;w
1;1;10
1;2;9
1;6;9
4;1;8
4;3;7
5;3;7
5;4;7
6;3;6'
))
res <- greedy(dta$x, dta$y, dta$w)
expect_equal(res, c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))

res <- greedy(dta$x, dta$y, dta$w, n = 999)
expect_equal(res, c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))

res <- greedy(dta$x, dta$y, dta$w, n = 2)
expect_equal(res, c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))

res <- greedy(dta$x, dta$y, dta$w, n = 3)
expect_equal(res, c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))

res <- greedy(dta$x, dta$y, dta$w, m = 999)
expect_equal(res, c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))

res <- greedy(dta$x, dta$y, dta$w, m = 2)
expect_equal(res, c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))

res <- greedy(dta$x, dta$y, dta$w, n = 999, m = 999)
expect_equal(res, c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

expect_error(
res <- greedy(dta$x, dta$y, dta$w, n = 999, m = 999, 
  include_ties = TRUE)
)


# Regression tests
# Issue 26
res <- greedy(1:5, c(1,1,2,2,2), rep(1,5), n = 2, m = 2)
expect_equal(res, c(TRUE, TRUE, TRUE, TRUE, FALSE))


message("Testing greedy() successful")

