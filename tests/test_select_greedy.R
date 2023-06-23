library(reclin2)
source("helpers.R")

pairs <- data.table(
  .x = c(3,3,1,2,3,2), 
  .y = c(4,5,1,2,1,3),
  score = c(1,5,10,5,4,1)
)
class(pairs) <- c("pairs", class(pairs))

t <- select_greedy(pairs, "select", "score")
expect_equal(t$select, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))

t <- select_greedy(pairs, "select", "score", threshold = 20)
expect_equal(t$select, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))

t <- select_greedy(pairs, "select", "score", threshold = 10)
expect_equal(t$select, c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))

# Test empty set pairs; regression test
t <- select_greedy(pairs[FALSE,], "select", "score", threshold = 10)
expect_equal(t$select, logical(0))

