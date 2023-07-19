
source("helpers.R")

expect_equal_pairs <- function(x, y) {
  setkey(x, .x, .y)
  setkey(y, .x, .y)
  expect_equal(names(x), names(y))
  for (col in names(x))
    expect_equal(x[[col]], y[[col]], attributes = FALSE)
}

library(reclin2)
library(parallel)

# Prepare data
data(linkexample1)
data(linkexample2)
linkexample1$postcode[1] <- NA
linkexample1$postcode[3] <- "6789 XY"

# What the result should look like
pairs_ref <- data.table(
  .x = c(1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L), 
  .y = c(1L, 1L, 2L, 3L, 3L, 4L, 5L, 1L, 2L, 3L, 1L, 2L, 3L, 4L, 5L), 
  firstname = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, 
      FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE),
  lastname = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, 
    FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
)

# Regular pairs
pairs1 <- pair_blocking(linkexample1, linkexample2, on = "postcode")
pairs2 <- pair_blocking(linkexample1, linkexample2, on = "lastname")
pairs <- merge_pairs(pairs1, pairs2)
compare_pairs(pairs, on = c("firstname", "lastname"), inplace = TRUE)
expect_equal_pairs(pairs, pairs_ref)

compare_pairs(pairs1, on = c("firstname", "lastname"), inplace = TRUE)
compare_pairs(pairs2, on = c("address", "lastname"), inplace = TRUE)
pairs <- merge_pairs(pairs1, pairs2)
expect_equal(sort(names(pairs)), c(".x", ".y", "address", "firstname", "lastname"))
expect_equal(is.na(pairs$address), !is.na(pairs$firstname))



# Cluster pairs
library(parallel)
cl <- makeCluster(2)
pairs1c <- cluster_pair_blocking(cl, linkexample1, linkexample2, on = "postcode", name="a")
pairs2c <- cluster_pair_blocking(cl, linkexample1, linkexample2, on = "lastname", name="b")
pairsc <- merge_pairs(pairs1c, pairs2c)
compare_pairs(pairsc, on = c("firstname", "lastname"), inplace = TRUE)
pairsc_local <- cluster_collect(pairsc)
expect_equal_pairs(pairsc_local, pairs_ref)

compare_pairs(pairs1c, on = c("firstname", "lastname"))
compare_pairs(pairs2c, on = c("address", "lastname"))
pairsc <- merge_pairs(pairs1c, pairs2c)
pairsc_local <- cluster_collect(pairsc)
expect_equal(sort(names(pairsc_local)), c(".x", ".y", "address", "firstname", "lastname"))
expect_equal(is.na(pairsc_local$address), !is.na(pairsc_local$firstname))

stopCluster(cl)

pairs1 <- pair_blocking(linkexample1, linkexample2, on = "postcode")
pairs2 <- pair_blocking(linkexample1, linkexample2, on = "lastname")
pairs2 <- pairs2[FALSE, ]
pairs <- merge_pairs(pairs1, pairs2)
compare_pairs(pairs, on = c("firstname", "lastname"), inplace = TRUE)
compare_pairs(pairs1, on = c("firstname", "lastname"), inplace = TRUE)
expect_equal_pairs(pairs1, pairs)


