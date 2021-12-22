#' Collect pairs from cluster nodes
#'
#' @param pairs an object or type \code{cluster_pairs} as created for example by
#'   \code{\link{cluster_pair}}.
#' @param clear remove the pairs from the cluster nodes
#'
#' @return
#' Returns an object of type \code{pairs} which is a \code{data.table}. This
#' object can be used as a regular (non-cluster) set of pairs
#' 
#' @examples
#' library(parallel)
#' data("linkexample1", "linkexample2")
#' cl <- makeCluster(2)
#' pairs <- cluster_pair(cl, linkexample1, linkexample2)
#' local_pairs <- cluster_collect(pairs, clear = TRUE)
#' stopCluster(cl)
#'
#' @importFrom parallel clusterCall
#' @import data.table
#' @export
cluster_collect <- function(pairs, clear = FALSE) {
  # Collect pairs
  tmp <- clusterCall(pairs$cluster, function(name, clear) {
    env <- reclin_env[[name]]
    pairs <- env$pairs
    x <- attr(pairs, "x")
    pairs$.x <- x$.id[pairs$.x]
    if (clear) {
      env$pairs <- NULL
      gc()
    }
    pairs
  }, name = pairs$name, clear = clear)
  p <- rbindlist(tmp)
  # x has been split; combine again into one dataset and add to pairs
  x <- lapply(tmp, function(d) attr(d, "x"))
  x <- rbindlist(x)
  attr(p, "x") <- x
  # Build a list of attributes we want to copy from the data.tables in tmp to 
  # the new pairs list
  attr_ignore <- c("x", "names", "row.names", "class")
  attr_names <- names(attributes(tmp[[1]]))
  attr_names <- setdiff(attr_names, attr_ignore)
  attr_names <- attr_names[!grepl("^\\.", attr_names)]
  # Copy/set attributes
  for (name in attr_names) {
    attr(p, name) <- attr(tmp[[1]], name)
  }
  class(p) <- c("pairs", class(p))
  p
}

