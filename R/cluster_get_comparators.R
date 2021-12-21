
#' @importFrom parallel clusterCall
get_comparators.cluster_pairs <- function(pairs) {
  comparators <- clusterCall(pairs$cluster, function(name) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    env <- reclin_env[[name]]
    get_comparators(env$pairs)
  }, name = pairs$name)
  # we now have a set of comparators for each cluster node; they 
  # should all be the same: returns the first one
  comparators[[1]]
}

