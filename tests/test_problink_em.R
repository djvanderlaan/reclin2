

library(reclin2)
source("helpers.R")


# ===============================================
# Regression test for issue #23
# Extreme example where earlier version ended up with m-probabilities equal to 0; 
# which resulted in divide by 0 -> NaN -> error. 
# This has been fixed. The m-probabilties are now truncated and a warning is issued
# as this results in a wrong solution. 

ex1 <- data.frame(
    a = "aaaaa",
    b = "zzzzz"
  )

ex2 <- data.frame(
    a = "bbbbb",
    b = "zzzzz"
  )

pairs <- pair(ex1, ex2)
pairs <- compare_pairs(pairs, on = c("a", "b"), default_comparator = cmp_jarowinkler())
expect_warning(
m <- problink_em(~ a + b, data = pairs)
)
