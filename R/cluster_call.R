#' Call a function on each of the worker nodes and pass it the pairs
#'
#' @param pairs an object or type \code{cluster_pairs} as created for example by
#'   \code{\link{cluster_pair}}.
#' @param fun a function to call on each of the worker nodes. See details on the
#'   arguments of this function. 
#' @param ... additional arguments are passed on to \code{fun}.
#' 
#' @details
#' The function will have to accept the following arguments as its first three
#' arguments:
#' 
#' \describe{
#'   \item{pairs}{the \code{data.table} with the pairs of the worker node.}
#'   \item{x}{a \code{data.table} with the portion of \code{x} present on the 
#'     worker node.}
#'   \item{y}{a \code{data.table} with \code{y}.}
#' }
#' 
#' @return
#' The function will return a list with for each worker the result of the
#' function call. When the functions return \code{NULL} the result is returned
#' invisibly. Because the result is returned to main node, make sure you don't
#' accidentally return all pairs. If you don't want to return anything end your
#' function with \code{NULL}.
#' 
#' @examples
#' # Generate some pairs
#' library(parallel)
#' data("linkexample1", "linkexample2")
#' cl <- makeCluster(1)
#' pairs <- cluster_pair(cl, linkexample1, linkexample2)
#' compare_pairs(pairs, c("lastname", "firstname", "address", "sex"))
#' 
#' # Add a new column to pairs
#' cluster_call(pairs, function(pairs, ...) {
#'   pairs[, name := firstname & lastname]
#'   # we don't want to return the pairs; so make sure to return something
#'   # else
#'   NULL
#' })
#' 
#' # Get the number of pairs on each node
#' lenghts <- cluster_call(pairs, function(pairs, ...) {
#'   nrow(pairs)
#' })
#' lengths <- unlist(lenghts)
#' lenghts
#' 
#' # Cleanup
#' stopCluster(cl)
#' 
#' @export
cluster_call <- function(pairs, fun, ...) {
  res <- clusterCall(pairs$cluster, function(name, fun, ...) {
    env <- reclin_env[[name]]
    pairs <- env$pairs
    x <- attr(pairs, "x")
    y <- attr(pairs, "y")
    fun(pairs, x, y, ...)
  }, pairs$name, fun, ...)
  if (all(sapply(res, function(x) is.null(x) || length(x) == 0)))
    invisible(res) else res
}

