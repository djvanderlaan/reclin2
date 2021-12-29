#' Call a function on each of the worker nodes to modify the pairs on the node
#'
#' @param pairs an object or type \code{cluster_pairs} as created for example by
#'   \code{\link{cluster_pair}}.
#' @param fun a function to call on each of the worker nodes. See details on the
#'   arguments of this function. 
#' @param ... additional arguments are passed on to \code{fun}.
#' @param new_name name of new object to assign the pairs to on the cluster
#'   nodes.
#' 
#' @details
#' The function will have to accept the following arguments as its first three
#' arguments:
#' 
#' \describe{
#'   \item{pairs}{the \code{data.table} with the pairs of the worker node. }
#'   \item{x}{a \code{data.table} with the portion of \code{x} present on the 
#'     worker node.}
#'   \item{y}{a \code{data.table} with \code{y}. }
#' }
#'
#' The function should either return a \code{data.table} with the new pairs, or
#' \code{NULL}. When a \code{data.table} is returned this values will replace
#' the pairs when \code{new_name} is missing or create new pairs in the
#' environment \code{new_name}. When the function returns \code{NULL} it is
#' assumed that the function modified the pairs by reference (e.g. using
#' \code{pairs[, new_var := new_val]}). Note that this also means that
#' \code{new_name} is ignored.
#' 
#' @return
#' Will return a \code{cluster_pairs} object. When \code{new_name} is not given
#' it will return the input \code{pairs} invisibly. Otherwise it will return a
#' new \code{cluster_pairs} object.
#' 
#' @examples
#' # Generate some pairs
#' library(parallel)
#' data("linkexample1", "linkexample2")
#' cl <- makeCluster(2)
#' pairs <- cluster_pair(cl, linkexample1, linkexample2)
#' compare_pairs(pairs, c("lastname", "firstname", "address", "sex"))
#' 
#' # Create a new set of pairs containing a random sample of the original
#' # pairs.
#' sample <-  cluster_call(pairs, new_name = "sample", function(pairs, ...) {
#'   sel <- sample(nrow(pairs), round(nrow(pairs)*0.1))
#'   pairs[sel, ]
#' })
#' 
#' # Cleanup
#' stopCluster(cl)
#' 
#' @export
cluster_modify_pairs <- function(pairs, fun, ..., new_name = NULL) {
  res <- clusterCall(pairs$cluster, function(name, new_name, fun, ...) {
    env <- reclin_env[[name]]
    pairs <- env$pairs
    x <- attr(pairs, "x")
    y <- attr(pairs, "y")
    res <- fun(pairs, x, y, ...)
    if (is.data.table(res)) {
      # We have a new version of the pairs
      if (!is.null(new_name)) {
        reclin_env[[new_name]] <- new.env()
        new_env <- reclin_env[[new_name]]
        new_env$pairs <- res
      } else {
        env$pairs <- res
      }
    } else if (!is.null(res)) {
      stop("fun should either return a data.table with the new pairs or return ",
        "NULL to indicate that the pairs have been modified by reference.")
    }
    NULL
  }, pairs$name, new_name, fun, ...)
  if (!missing(new_name) && !is.null(new_name)) {
    pairs$name <- new_name
    pairs
  } else {
    invisible(pairs)
  }
}

