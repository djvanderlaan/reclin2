#' Merge two sets of pairs into one
#'
#' @param pairs1 the first set of pairs
#' @param pairs2 the second set of pairs
#' @param name name of new object to assign the pairs to on the cluster
#'   nodes.
#' @param ... for \code{rbind} the \code{pairs} or \code{cluster_pairs} objects 
#'   the need to be combined; for \code{merge_pairs} these are passed on to 
#'   other methods.
#'
#' @details
#' The function will give an error when the two sets of pairs have different values
#' for \code{attr(pairs1, "x")} and \code{attr(pairs1, "y")}. When these attributes
#' are missing the code will not generate an error; the user is then
#' responsible for ensuring that the indices in \code{pairs1} and \code{pairs2}
#' refer to the same datasets.
#'
#' @return
#' Returns a \code{pairs} or \code{cluster_pairs} object where both sets of pairs
#' are combined. Duplicate pairs are removed. 
#' 
#' In case of \code{merge_pairs.cluster_pairs}, \code{merge_pairs.pairs} is called on
#' each cluster node and the resulting pairs are assigned to \code{name} in
#' the environment \code{reclin_env}. 
#' 
#' @rdname merge_pairs
#' @export
merge_pairs <- function(pairs1, pairs2, ...) {
  UseMethod("merge_pairs")
}

#' @rdname merge_pairs
#' @export
merge_pairs.pairs <- function(pairs1, pairs2, ...) {
  if (!isTRUE(all.equal(attr(pairs1, "x"), attr(pairs2, "x")))) 
    stop("The dataset x of the first set of pairs is not equal to the dataset ",
      "x of the second set.")
  if (!isTRUE(all.equal(attr(pairs1, "y"), attr(pairs2, "y")))) 
    stop("The dataset x of the first set of pairs is not equal to the dataset ",
      "x of the second set.")
  res <- data.table::rbindlist(list(pairs1, pairs2), use.names = TRUE, fill = TRUE)
  setkey(res, .x, .y)
  res <- unique(res)
  setattr(res, "class", c("pairs", class(res)))
  setattr(res, "x", attr(pairs1, "x"))
  setattr(res, "y", attr(pairs1, "y"))
  res
}

#' @rdname merge_pairs
#' @export
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

