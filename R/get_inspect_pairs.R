#' Get a subset of pairs to inspect
#'
#' @param pairs \code{\link{data.table}} with pairs.
#' @param variable name of variable to base the selection on; should be a variable
#'   with the similarity score of the pairs.
#' @param threshold the threshold around which to select pairs. Used when position 
#'   is not given.
#' @param position select pairs around this position (based on order of 
#'   \code{variable}), e.g. \code{position = 1} will select the pairs with the 
#'   highest similarity score.
#' @param n number of pairs to select. Pairs are selected symmetric around the 
#'   theshold. 
#' @param x \code{data.table} with one half of the pairs.
#' @param y \code{data.table} with the other half of the pairs.
#'
#' @return 
#' Returns a list with elements \code{pairs} with the selected pairs; 
#' \code{x} records from \code{x} corresponding to the pairs; \code{y} records
#' from \code{y} corresponding to the pairs; \code{position} position of the 
#' selected pairs; \code{index} index of the pairs in \code{pairs}.
#'
#' @export
get_inspect_pairs <- function(pairs, variable, threshold, position = NULL, 
    n = 11,  x = attr(pairs, 'x'), y = attr(pairs, 'y')) {
  o <- order(-pairs[[variable]])
  if (missing(position) || is.null(position)) {
    i <- which.min(abs(pairs[[variable]] - threshold))
    position <- which(i == o)
  }
  range <- 1:n - ceiling(n/2)
  sel   <- position + range
  sel   <- sel[(sel > 0) & (sel < length(o))]
  p     <- pairs[o[sel], ]

  structure(
    list(pairs = p, x = x[p$.x], y = y[p$.y], position = sel, index = o[sel]),
    class = "inspect_pairs")
}


#' @export
print.inspect_pairs <- function(x, ...) {
  n <- nrow(x$pairs)
  for (i in seq_len(n)) {
    cat("\n\033[33;7;1mPair ==============================================================\033[0m\n")
    cat("Index    = ", x$index[i], "\n")
    cat("Position = ", x$position[i], "\n") 
    print(as.data.table(x$pairs[i, ]))
    cat("\n\033[32;1m==== Corresponding record from x ====\033[0m\n")
    print(x$x[i, ])
    cat("\n\033[31;1m==== Corresponding record from y ====\033[0m\n")
    print(x$y[i, ])
  }
}
