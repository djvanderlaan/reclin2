#' Compare pairs on a set of variables common in both data sets
#'
#' @param pairs \code{\link{data.table}} with pairs. Should contain the columns 
#'   \code{.x} and \code{.y}.
#' @param on character vector of variables that should be compared. 
#' @param comparators named list of functions with which the variables are compared. 
#'   This function should accept two vectors. Function should either return a vector
#'   or a \code{data.table} with multiple columns.
#' @param default_comparator variables for which no comparison function is defined using
#'   \code{comparators} is compares with the function \code{default_comparator}.
#' @param x \code{data.table} with one half of the pairs.
#' @param y \code{data.table} with the other half of the pairs.
#' @param inplace logical indicating whether \code{pairs} should be modified in place. When
#'   pairs is large this can be more efficient.
#' @param ... Ignored for now
#'
#' @details
#' It is assumed the variables in \code{on} are present in both \code{x} and \code{y}. Variables
#' with the same names are added to pairs. 
#' When the \code{comparator} returns a \code{data.table} multiple columns are added to \code{pairs}. 
#' The names of these columns are \code{variable} pasted together with the names of 
#' the \code{data.table} returned by \code{comparator} (separated by "_"). 
#' 
#' @return
#' Returns the \code{data.table} \code{pairs} with one or more columns added. 
#' 
#' @rdname compare_pairs
#' @export 
compare_pairs <- function(pairs, on, comparators = list(default_comparator), 
    default_comparator = identical(), ...) {
  UseMethod("compare_pairs", pairs)
}


#' @rdname compare_pairs
#' @export
compare_pairs.pairs <- function(pairs, on, comparators = list(default_comparator), 
    default_comparator = identical(), x = attr(pairs, 'x'), y = attr(pairs, 'y'), 
    inplace = FALSE, ...) {
  if (missing(on) && !missing(on)) on <- names(comparators)
  if (missing(on) || is.null(on)) stop("on is missing.")
  if (!all(on %in% names(x)))
    stop("Not all variables in on are present in x.")
  if (!all(on %in% names(y))) 
    stop("Not all variables in on are present in y.")
  comparators <- extend_to(on, comparators, default = default_comparator)  
  # Compare
  if (!inplace) pairs <- copy(pairs)
  for (var in on) {
    compare_vars(pairs, var, comparator = comparators[[var]], x = x, y = y, 
      inplace = TRUE)
  }
  if (inplace) invisible(pairs) else pairs
}

