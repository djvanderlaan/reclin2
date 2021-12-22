#' Generate all possible pairs using multiple processes
#'
#' Generates all combinations of records from \code{x} and \code{y}.
#'
#' @param cluster a cluster object as created by \code{\link[parallel]{makeCluster}}
#'   from \code{parallel} or \code{\link[snow]{makeCluster}} from \code{snow}.
#' @param x first \code{data.frame}
#' @param y second \code{data.frame}. Ignored when \code{deduplication = TRUE}.
#' @param deduplication generate pairs from only \code{x}. Ignore \code{y}. This 
#'   is usefull for deduplication of \code{x}.
#' @param name the name of the resulting object to create locally on the different
#'   R processes.
#'
#' @details
#' Generating (all) pairs of the records of two data sets, is usually the first 
#' step when linking the two data sets. 
#'
#' \code{x} is split into \code{length{cluster}} parts which are distributed
#' over the worker nodes. \code{y} is copied to each of the nodes. On the nodes
#' then \code{\link{pair}} is called. The pairs are stored in the global
#' object \code{reclin_env} on the nodes in the variable \code{name}. The pairs
#' can then be further processes using functions such as
#' \code{\link{compare_pairs}}, and \code{\link{tabulate_patterns}}. The function
#' \code{\link{cluster_collect}} collects the pairs from each of the nodes.
#'  
#' @return 
#' A object of type \code{cluster_pairs} which is a \code{list} containing the
#' cluster and the name of the pairs object on the cluster nodes. For the pairs
#' objects created on the nodes see the documentation of \code{\link{pair}}.
#'
#' @seealso
#' \code{\link{cluster_pair_blocking}} and \code{\link{cluster_pair_minsim}} are
#' other methods to generate pairs. 
#'
#' @examples
#' library(parallel)
#' data("linkexample1", "linkexample2")
#' cl <- makeCluster(2)
#' pairs <- cluster_pair(cl, linkexample1, linkexample2)
#' stopCluster(cl)
#'
#' @importFrom parallel clusterApply
#' @import data.table
#' @export
cluster_pair <- function(cluster, x, y, deduplication = FALSE, name = "default") {
  # TODO: handle deduplication
  x <- as.data.table(x)
  y <- as.data.table(y)
  # Split x into a length(cluster) groups
  group <- floor(seq_len(nrow(x))/(nrow(x)+1)*length(cluster))
  group <- sample(group)
  idx <- split(seq_len(nrow(x)), group)
  x <- split(x, group)
  for (i in seq_along(x)) x[[i]]$.id <- idx[[i]]
  # Copy data to cluster
  clusterApply(cluster, x, function(name, x, y) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    # environment in which to store all data
    if (!exists("reclin_env")) reclin_env <<- new.env()
    # TODO: warnings are not returned to main
    if (exists(name, envir = reclin_env)) 
      warning("'", name, "' already exists; overwriting.")
    reclin_env[[name]] <- new.env()
    reclin_env[[name]]$pairs <- pair(x, y)
    TRUE
  }, name = name, y = y)
  structure(list(cluster = cluster, name = name), class = "cluster_pairs")
}


