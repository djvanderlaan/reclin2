
#' @importFrom parallel clusterApply
#' @import data.table
#' @export
cluster_pair_minsim<- function(cluster, x, y, on, minsim = 0.0, 
    comparators = list(default_comparator), default_comparator = identical(), 
    keep_simsum = TRUE, name = "default") {
  x <- as.data.table(x)
  y <- as.data.table(y)
  # Split x into a length(cluster) groups
  group <- floor(seq_len(nrow(x))/(nrow(x)+1)*length(cl))
  group <- sample(group)
  idx <- split(seq_len(nrow(x)), group)
  x <- split(x, group)
  for (i in seq_along(x)) x[[i]]$.id <- idx[[i]]
  # Copy data to cluster
  clusterApply(cluster, x, function(name, x, y, on, minsim = minsim, 
      comparators = comparators, default_comparator = default_comparator, 
      keep_simsum = keep_simsum) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    # environment in which to store all data
    if (!exists("reclin_env")) reclin_env <<- new.env()
    # TODO: warnings are not returned to main
    if (exists(name, envir = reclin_env)) 
      warning("'", name, "' already exists; overwriting.")
    reclin_env[[name]] <- new.env()
    reclin_env[[name]]$pairs <- pair_minsim(x, y, on = on, minsim = minsim, 
      comparators = comparators, default_comparator = default_comparator, 
      keep_simsum = keep_simsum)
    TRUE
  }, name = name, y = y, on = on, minsim = minsim, comparators = comparators, 
    default_comparator = default_comparator, keep_simsum = keep_simsum)
  structure(list(cluster = cluster, name = name), class = "cluster_pairs")
}

