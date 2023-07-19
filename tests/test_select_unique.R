library(reclin2)
library(parallel)
source("helpers.R")

pairs <- data.table(
  .x  = c(3,3,1,2,3,2,4), 
  .y  = c(4,5,1,2,1,3,6),
  sel = c(1,1,1,1,1,1,1) == 1
)
class(pairs) <- c("pairs", class(pairs))

pairs$sel <- c(1,1,1,1,1,1,1) == 1
tmp <- select_unique(pairs, "sel2", preselect = "sel")
expect_equal(names(tmp), c(names(pairs), "sel2"))
expect_equal(tmp$sel2, c(0,0,0,0,0,0,1) == 1)

pairs$sel <- c(1,1,1,1,1,1,1) == 1
tmp <- select_unique(pairs, "sel2")
expect_equal(names(tmp), c(names(pairs), "sel2"))
expect_equal(tmp$sel2, c(0,0,0,0,0,0,1) == 1)

pairs$sel <- c(1,0,1,1,0,1,1) == 1
tmp <- select_unique(pairs, "sel2")
expect_equal(tmp$sel2, c(0,0,0,0,0,0,1) == 1)
tmp <- select_unique(pairs, "sel2", preselect = "sel")
expect_equal(tmp$sel2, c(1,0,1,0,0,0,1) == 1)

pairs$sel <- c(0,0,0,0,0,0,0) == 1
tmp <- select_unique(pairs, "sel2")
expect_equal(tmp$sel2, c(0,0,0,0,0,0,1) == 1)
tmp <- select_unique(pairs, "sel2", preselect = "sel")
expect_equal(tmp$sel2, c(0,0,0,0,0,0,0) == 1)


# === N & M ARGUMENTS
pairs$sel <- c(1,1,1,1,1,1,1) == 1
tmp <- select_unique(pairs, "sel2", preselect = "sel", n = Inf)
expect_equal(tmp$sel2, c(0,0,1,0,0,0,1) == 1)
tmp <- select_unique(pairs, "sel2", preselect = "sel", m = Inf)
expect_equal(tmp$sel2, c(1,1,0,1,0,1,1) == 1)
tmp <- select_unique(pairs, "sel2", preselect = "sel", m = 2)
expect_equal(tmp$sel2, c(0,0,0,1,0,1,1) == 1)
tmp <- select_unique(pairs, "sel2", preselect = "sel", n = 2, m = 2)
expect_equal(tmp$sel2, c(0,0,1,1,0,1,1) == 1)
tmp <- select_unique(pairs, "sel2", preselect = "sel", n = Inf, m = Inf)
expect_equal(tmp$sel2, c(1,1,1,1,1,1,1) == 1)

# === INPLACE ARGUMENT
pairs$sel <- c(1,1,1,1,1,1,1) == 1
select_unique(pairs, "sel2", preselect = "sel", n = Inf, inplace = TRUE)
expect_equal(pairs$sel2, c(0,0,1,0,0,0,1) == 1)
pairs[, sel2 := NULL]

# === ID_X and ID_Y
x <- data.table(id = c(1,2,3,4))
setattr(pairs, "x", x)
y <- data.table(id = c(1,2,3,4,5,6))
setattr(pairs, "y", y)

pairs$sel <- c(1,1,1,1,1,1,1) == 1
tmp <- select_unique(pairs, "sel2", preselect = "sel", 
  id_x = "id", id_y = "id")
expect_equal(tmp$sel2, c(0,0,0,0,0,0,1) == 1)

x <- data.table(id = c(1,2,3,4))
setattr(pairs, "x", x)
y <- data.table(id = c(1,2,3,4,5,5))
setattr(pairs, "y", y)

pairs$sel <- c(1,1,1,1,1,1,1) == 1
tmp <- select_unique(pairs, "sel2", preselect = "sel", 
  id_x = "id", id_y = "id")
expect_equal(tmp$sel2, c(0,0,0,0,0,0,0) == 1)

id_x <- x$id[pairs$.x]
id_y <- x$id[pairs$.y]
tmp <- select_unique(pairs, "sel2", preselect = "sel", 
  id_x = id_x, id_y = id_y)
expect_equal(tmp$sel2, c(0,0,0,0,0,0,0) == 1)

expect_error(
tmp <- select_unique(pairs, "sel2", preselect = "sel", 
  id_x = 1:2, id_y = id_y)
)
expect_error(
tmp <- select_unique(pairs, "sel2", preselect = "sel", 
  id_x = id_x, id_y = 1:3)
)

# === EDGE CASES
# Empty pairs
pairs$sel <- c(1,1,1,1,1,1,1) == 1
tmp <- select_unique(pairs[FALSE, ], "sel2", preselect = "sel", n = Inf)
expect_equal(names(tmp), c(names(pairs), "sel2"))
expect_equal(tmp$sel2, logical(0))

# === CLUSTER
x <- data.table(a = c(1,1,2,2), b = c(1,2,1,2))
y <- data.table(a = c(3,3,2,2), b = c(1,2,1,2))

cl <- makeCluster(2)
clusterEvalQ(cl, data.table::setDTthreads(1))

set.seed(103)
pairs <- cluster_pair_blocking(cl, x, y, on = "a")
compare_pairs(pairs, c("a", "b"))
score_simple(pairs, on = c("a", "b"), variable = "score")

expect_error(
  select_unique(pairs, "sel2")
)

stopCluster(cl)
