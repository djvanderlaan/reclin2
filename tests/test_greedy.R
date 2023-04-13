
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

res <- reclin2:::greedy(dta$x, dta$y, dta$w)
expect_equal(res, c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))

# Change order; greedy should sort
set.seed(10)
o <- sample(nrow(dta))
res <- reclin2:::greedy(dta$x[o], dta$y[o], dta$w[o])
expect_equal(res, c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)[o])

# Empty input
res <- reclin2:::greedy(dta$x[FALSE], dta$y[FALSE], dta$w[FALSE])
expect_equal(res, logical(0))

# Missing values
dta2 <- dta
dta2$w[1] <- NA
expect_error(
  res <- reclin2:::greedy(dta2$x, dta2$y, dta2$w)
)

# include_ties = TRUE
res <- reclin2:::greedy(dta$x, dta$y, dta$w, include_ties = TRUE)
expect_equal(res, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))

# include_ties = TRUE
# Change order; greedy should sort
set.seed(10)
o <- sample(nrow(dta))
res <- reclin2:::greedy(dta$x[o], dta$y[o], dta$w[o], include_ties = TRUE)
expect_equal(res, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)[o])

# include_ties = TRUE
# Empty input
res <- reclin2:::greedy(dta$x[FALSE], dta$y[FALSE], dta$w[FALSE], include_ties = TRUE)
expect_equal(res, logical(0))

# include_ties = TRUE
# Missing values
dta2 <- dta
dta2$w[1] <- NA
expect_error(
  res <- reclin2:::greedy(dta2$x, dta2$y, dta2$w, include_ties = TRUE)
)

message("Testing greedy() successful")

