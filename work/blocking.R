library(reclin2)

data(linkexample1)
data(linkexample2)

linkexample1
linkexample2

linkexample1$postcode[1] <- NA
linkexample1$postcode[3] <- "6789 XY"


res <- pair_minsim(linkexample1, linkexample2, on = c("postcode", "lastname"), minsim = 0.5)

data.table(
  .x = c(1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L), 
  .y = c(1L, 1L, 2L, 3L, 3L, 4L, 5L, 1L, 2L, 3L, 1L, 2L, 3L, 4L, 5L), 
  simsum = c(1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 2, 2)
)


x <- data.table(a = c(1,1,2,2), b = c(1,2,1,2))
y <- data.table(a = c(3,3,2,2), b = c(1,2,1,2))
x$a[1] <- NA

pairs <- pair_minsim(x, y, on = c("b", "a"), minsim = 1)
expect_equal(pairs$.x, c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L))
expect_equal(pairs$.y, c(1L, 3L, 2L, 4L, 1L, 3L, 4L, 2L, 3L, 4L))
expect_equal(pairs$simsum, c(1, 1, 1, 1, 1, 2, 1, 1, 1, 2))

dput(pairs)

data.table(
  .x = c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L), 
  .y = c(1L, 3L, 2L, 4L, 1L, 3L, 4L, 2L, 3L, 4L), 
  simsum = c(1, 1, 1, 1, 1, 2, 1, 1, 1, 2))

res <- pair_minsim(linkexample1, linkexample2, on = c("postcode", "lastname"), minsim = 0.5)


pairs1 <- pair_blocking(linkexample1, linkexample2, on = "postcode")
pairs2 <- pair_blocking(linkexample1, linkexample2, on = "lastname")



pkgload::load_all()

merge_pairs <- function(pairs1, pairs2, ...) {
  UseMethod("merge_pairs")
}
merge_pairs.pairs <- function(pairs1, pairs2, ...) {
  if (!isTRUE(all.equal(attr(pairs1, "x"), attr(pairs2, "x")))) 
    stop("The dataset x of the first set of pairs is not equal to the dataset ",
      "x of the second set.")
  if (!isTRUE(all.equal(attr(pairs1, "y"), attr(pairs2, "y")))) 
    stop("The dataset x of the first set of pairs is not equal to the dataset ",
      "x of the second set.")
  res <- rbind(pairs1, pairs2)
  setkey(res, .x, .y)
  res <- unique(res)
  setattr(res, "class", c("pairs", class(res)))
  setattr(res, "x", attr(pairs1, "x"))
  setattr(res, "y", attr(pairs1, "y"))
  res
}
merge_pairs.cluster_pairs <- function(pairs1, pairs2, 
    name = paste(pairs1$name, pairs2$name, sep="+"), ...) {
  if (!isTRUE(all.equal(pairs1$cluster, pairs2$cluster))) 
    stop("The two sets of pairs do not use the same cluster.")
  if (pairs1$name == pairs2$name)
    stop("The names of the two sets of pairs are equal.")
  clusterCall(pairs1$cluster, function(name1, name2, new_name) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    reclin_env <- reclin2:::reclin_env
    if (exists(new_name, envir = reclin_env)) 
      warning("'", new_name, "' already exists; overwriting.")
    env1 <- reclin_env[[name1]]
    env2 <- reclin_env[[name2]]
    reclin_env[[new_name]] <- new.env()
    new_env <- reclin_env[[new_name]]
    new_env$pairs <- merge_pairs(env1$pairs, env2$pairs)
    invisible(NULL)
  }, pairs1$name, pairs2$name, name)
  structure(list(cluster = pairs1$cluster, name = name),
    class = "cluster_pairs")
}

data(linkexample1)
data(linkexample2)
linkexample1$postcode[1] <- NA
linkexample1$postcode[3] <- "6789 XY"

pairs1 <- pair_blocking(linkexample1, linkexample2, on = "postcode")
pairs2 <- pair_blocking(linkexample1, linkexample2, on = "lastname")
pairs <- merge_pairs(pairs1, pairs2)
compare_pairs(pairs, on = c("firstname", "lastname"), inplace = TRUE)
pairs

library(parallel)
cl <- makeCluster(2)
pairs1c <- cluster_pair_blocking(cl, linkexample1, linkexample2, on = "postcode", name="a")
pairs2c <- cluster_pair_blocking(cl, linkexample1, linkexample2, on = "lastname", name="b")
pairsc <- merge_pairs(pairs1c, pairs2c)
compare_pairs(pairsc, on = c("firstname", "lastname"), inplace = TRUE)
pairsc_local <- cluster_collect(pairsc)


rbind.pairs <- function(...) {
  p <- list(...)
  if (length(p) < 1) stop("No arguments given.")
  if (length(p) == 1) return (p[[1]])
  res <- merge_pairs(p[[1]], p[[2]])
  if (length(p) > 2) {
    for (i in seq(3, length(p), by = 1)) 
      res <- merge_pairs(res, p[[i]])
  }
  res
}
rbind(pairs1, pairs2, pairs2)

rbind.cluster_pairs <- function(...) {
  p <- list(...)
  name <- paste(sapply(p, function(p) p$name), collapse="+")
  if (length(p) < 1) stop("No arguments given.")
  if (length(p) == 1) return (p[[1]])
  res <- merge_pairs(p[[1]], p[[2]], name = name)
  if (length(p) > 2) {
    for (i in seq(3, length(p), by = 1)) 
      res <- merge_pairs(res, p[[i]], name = name)
  }
  res
}
rbind(pairs1c, pairs2c, pairs2c)

p <- list(pairs1, pairs2, pairs1, pairs2)
Reduce(merge_pairs, p)


