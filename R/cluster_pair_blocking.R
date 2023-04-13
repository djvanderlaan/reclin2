#' Generate pairs using simple blocking using multiple processes
#'
#' Generates all combinations of records from \code{x} and \code{y} where the 
#' blocking variables are equal. 
#'
#' @param cluster a cluster object as created by \code{\link[parallel]{makeCluster}}
#'   from \code{parallel} or from the \code{snow} package.
#' @param x first \code{data.frame}
#' @param y second \code{data.frame}. Ignored when \code{deduplication = TRUE}.
#' @param on the variables defining the blocks or strata for which 
#'   all pairs of \code{x} and \code{y} will be generated.
#' @param deduplication generate pairs from only \code{x}. Ignore \code{y}. This 
#'   is usefull for deduplication of \code{x}.
#' @param name the name of the resulting object to create locally on the different
#'   R processes.
#'
#' @details
#' Generating (all) pairs of the records of two data sets, is usually the first 
#' step when linking the two data sets. However, this often results in a too 
#' large number of records. Therefore, blocking is usually applied. 
#'  
#' \code{x} is split into \code{length{cluster}} parts which are distributed
#' over the worker nodes. \code{y} is copied to each of the nodes. On the nodes
#' then \code{\link{pair_blocking}} is called. The pairs are stored in the global
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
#' \code{\link{cluster_pair}} and \code{\link{cluster_pair_minsim}} are 
#' other methods to generate pairs. 
#'
#' @examples
#' library(parallel)
#' data("linkexample1", "linkexample2")
#' cl <- makeCluster(2)
#' pairs <- cluster_pair_blocking(cl, linkexample1, linkexample2, "postcode")
#' stopCluster(cl)
#'
#' @importFrom parallel clusterApply
#' @import data.table
#' @export
cluster_pair_blocking <- function(cluster, x, y, on, deduplication = FALSE, name = "default") {
  x <- as.data.table(x)
  if (deduplication && !missing(y)) warning("y provided will be ignored.")
  y <- if (deduplication) x else as.data.table(y)
  # Split x into a length(cluster) groups
  group <- distribute_over_cluster(nrow(x), length(cluster))
  idx <- split(seq_len(nrow(x)), group)
  x <- split(x, group)
  for (i in seq_along(x)) x[[i]]$.id <- idx[[i]]
  # Copy data to cluster
  clusterApply(cluster, x, function(name, x, y, on, deduplication) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    # environment in which to store all data
    if (!exists("reclin_env")) reclin_env <<- new.env()
    # TODO: warnings are not returned to main
    if (exists(name, envir = reclin_env)) 
      warning("'", name, "' already exists; overwriting.")
    reclin_env[[name]] <- new.env()
    reclin_env[[name]]$pairs <- pair_blocking(x, y, on)
    # Handle deduplication; we cannot use the deduplication argument of 
    # the pair function
    if (deduplication) {
      ids <- x$.id[ reclin_env[[name]]$pairs$.x ]
      reclin_env[[name]]$pairs <- reclin_env[[name]]$pairs[.y > ids]
    }
    TRUE
  }, name = name, y = y, on = on, deduplication = deduplication)
  structure(list(cluster = cluster, name = name), class = "cluster_pairs", 
    blocking_on = on)
}

