
#' @rdname merge_pairs
#' @export
merge_pairs.cluster_pairs <- function(pairs1, pairs2, 
    name = paste(pairs1$name, pairs2$name, sep="+"), ...) {
  if (!isTRUE(all.equal(pairs1$cluster, pairs2$cluster))) 
    stop("The two sets of pairs do not use the same cluster.")
  if (pairs1$name == pairs2$name)
    stop("The names of the two sets of pairs are equal.")
  clusterCall(pairs1$cluster, function(name1, name2, new_name) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
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

#' @rdname merge_pairs
#' @export
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
