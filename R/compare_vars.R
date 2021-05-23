
#' Compare pairs on given variables
#'
#' @param pairs \code{\link{data.table}} with pairs. Should contain the columns 
#'   \code{.x} and \code{.y}.
#' @param variable character vector with name of resulting column name that is added
#'   to pairs.
#' @param on_x character vector with the column names from \code{x} on which to 
#'   compare. 
#' @param on_y character vector with the column names from \code{y} on which to 
#'   compare. 
#' @param comparator function with which the variables are compared. When \code{on_x} and 
#'   \code{on_y} have length 1, this function should accept two vectors. Otherwise
#'   it will receive two \code{data.tables}. Function should either return a vector
#'   or a \code{data.table} with multiple columns.
#' @param x \code{data.table} with one half of the pairs.
#' @param y \code{data.table} with the other half of the pairs.
#' @param inplace logical indicating whether \code{pairs} should be modified in place. When
#'   pairs is large this can be more efficient.
#'
#' @details
#' When \code{comparator} returns a \code{data.table} multiple columns are added to \code{pairs}. 
#' The names of these columns are \code{variable} pasted together with the names of 
#' the \code{data.table} returned by \code{comparator} (separated by "_"). 
#' 
#' @return
#' Returns the \code{data.table} \code{pairs} with one or more columns added. 
#' 
#' @export
compare_vars <- function(pairs, variable, on_x = variable, on_y = on_x, comparator = identical(), 
    x = attr(pairs, 'x'), y = attr(pairs, 'y'), inplace = FALSE) {
  xv <- x[pairs$.x, ..on_x]
  yv <- y[pairs$.y, ..on_y]
  # Compare
  res <- if (ncol(xv) == 1 && ncol(yv) == 1) 
    comparator(xv[[1]], yv[[1]]) else comparator(xv, yv)
  attr(res, "comparator") <- comparator
  attr(res, "on_x") <- on_x
  attr(res, "on_y") <- on_y
  # Assign result of comparison to pairs
  if (is.data.table(res)) {
    for (col in names(res)) {
      v <- paste0(variable, "_", col)
      if (inplace) pairs[, (v) := res[[col]]] else pairs[[v]] <- res[[col]]
    }
  } else {
    if (inplace) pairs[, (variable) := res] else pairs[[variable]] <- res
  }
  # Todo store comparison function and variables on which was compared
  if (inplace) invisible(pairs) else pairs
}

