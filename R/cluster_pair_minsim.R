#' Generate pairs with a minimal similarity using multiple processes
#'
#' Generates all combinations of records from \code{x} and \code{y} where the 
#' blocking variables are equal. 
#'
#' @param cluster a cluster object as created by \code{\link[parallel]{makeCluster}}
#'   from \code{parallel} or \code{makeCluster} from \code{snow}.
#' @param x first \code{data.frame}
#' @param y second \code{data.frame}. Ignored when \code{deduplication = TRUE}.
#' @param on the variables defining the blocks or strata for which 
#'   all pairs of \code{x} and \code{y} will be generated.
#' @param minsim minimal similarity score.
#' @param on_blocking variables for which the pairs have to match.
#' @param comparators named list of functions with which the variables are compared. 
#'   This function should accept two vectors. Function should either return a vector
#'   or a \code{data.table} with multiple columns.
#' @param default_comparator variables for which no comparison function is defined using
#'   \code{comparators} is compares with the function \code{default_comparator}.
#' @param keep_simsum add a variable \code{minsim} to the result with the similarity 
#'   score of the pair.
#' @param deduplication generate pairs from only \code{x}. Ignore \code{y}. This 
#'   is usefull for deduplication of \code{x}.
#' @param name the name of the resulting object to create locally on the different
#'   R processes.
#'
#' @details
#' Generating (all) pairs of the records of two data sets, is usually the first 
#' step when linking the two data sets. However, this often results in a too 
#' large number of records. \code{pair_minsim} will only keep pairs with a 
#' similarity score equal or larger than \code{minsim}. The similarity score is
#' calculated by summing the results of the comparators for all variables 
#' of \code{on}.
#'
#' \code{x} is split into \code{length{cluster}} parts which are distributed
#' over the worker nodes. \code{y} is copied to each of the nodes. On the nodes
#' then \code{\link{cluster_pair_minsim}} is called. The pairs are stored in the global
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
#' \code{\link{cluster_pair}} and \code{\link{cluster_pair_blocking}} are 
#' other methods to generate pairs. 
#'
#' @examples
#' library(parallel)
#' data("linkexample1", "linkexample2")
#' cl <- makeCluster(2)
#' \dontshow{clusterEvalQ(cl, data.table::setDTthreads(1))}
#' Generate all possible pairs using multiple processes
#'
#' Generates all combinations of records from \code{x} and \code{y}.
#'
#' @param cluster a cluster object as created by \code{\link[parallel]{makeCluster}}
#'   from \code{parallel} or from the \code{snow} package.
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
#' \dontshow{clusterEvalQ(cl, data.table::setDTthreads(1))}
#' pairs <- cluster_pair(cl, linkexample1, linkexample2)
#' stopCluster(cl)
#'
#' @importFrom parallel clusterApply
#' @import data.table
#' @export
cluster_pair <- function(cluster, x, y, deduplication = FALSE, name = "default") {
  x <- as.data.table(x)
  if (deduplication && !missing(y)) warning("y provided will be ignored.")
  y <- if (deduplication) x else as.data.table(y)
  # Split x into a length(cluster) groups
  group <- distribute_over_cluster(nrow(x), length(cluster))
  idx <- split(seq_len(nrow(x)), group)
  x <- split(x, group)
  for (i in seq_along(x)) x[[i]]$.id <- idx[[i]]
  # Copy data to cluster
  clusterApply(cluster, x, function(name, x, y, deduplication) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    # environment in which to store all data
    if (!exists("reclin_env")) reclin_env <<- new.env()
    # TODO: warnings are not returned to main
    if (exists(name, envir = reclin_env)) 
      warning("'", name, "' already exists; overwriting.")
    reclin_env[[name]] <- new.env()
    reclin_env[[name]]$pairs <- pair(x, y)
    # Handle deduplication; we cannot use the deduplication argument of 
    # the pair function
    if (deduplication) {
      ids <- x$.id[ reclin_env[[name]]$pairs$.x ]
      reclin_env[[name]]$pairs <- reclin_env[[name]]$pairs[.y > ids]
    }
    TRUE
  }, name = name, y = y, deduplication = deduplication)
  structure(list(cluster = cluster, name = name), class = "cluster_pairs")
}

#' # Either address or postcode has to match to keep a pair
#' pairs <- cluster_pair_minsim(cl, linkexample1, linkexample2, 
#'    on = c("postcode", "address"), minsim = 1)
#' stopCluster(cl)
#'
#' @importFrom parallel clusterApply
#' @import data.table
#' @export
cluster_pair_minsim<- function(cluster, x, y, on, minsim = 0.0, 
    on_blocking = character(0), comparators = list(default_comparator), 
    default_comparator = cmp_identical(), keep_simsum = TRUE, 
    deduplication = FALSE, name = "default") {
  x <- as.data.table(x)
  if (deduplication && !missing(y)) warning("y provided will be ignored.")
  y <- if (deduplication) x else as.data.table(y)
  # Split x into a length(cluster) groups
  group <- distribute_over_cluster(nrow(x), length(cluster))
  idx <- split(seq_len(nrow(x)), group)
  x <- split(x, group)
  for (i in seq_along(x)) x[[i]]$.id <- idx[[i]]
  # Copy data to cluster
  clusterApply(cluster, x, function(name, x, y, on, minsim, on_blocking, comparators, 
      default_comparator, keep_simsum, deduplication) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    # environment in which to store all data
    if (!exists("reclin_env")) reclin_env <<- new.env()
    # TODO: warnings are not returned to main
    if (exists(name, envir = reclin_env)) 
      warning("'", name, "' already exists; overwriting.")
    reclin_env[[name]] <- new.env()
    reclin_env[[name]]$pairs <- pair_minsim(x, y, on = on, minsim = minsim, 
      on_blocking = on_blocking, comparators = comparators, 
      default_comparator = default_comparator, keep_simsum = keep_simsum)
    # Handle deduplication; we cannot use the deduplication argument of 
    # the pair function
    if (deduplication) {
      ids <- x$.id[ reclin_env[[name]]$pairs$.x ]
      reclin_env[[name]]$pairs <- reclin_env[[name]]$pairs[.y > ids]
    }
    TRUE
  }, name = name, y = y, on = on, on_blocking = on_blocking, minsim = minsim, 
    comparators = comparators, default_comparator = default_comparator, 
    keep_simsum = keep_simsum, deduplication = deduplication)
  structure(list(cluster = cluster, name = name), class = "cluster_pairs")
}

