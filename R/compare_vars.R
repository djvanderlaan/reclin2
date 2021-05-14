
#' Compare pairs on given variables
#'
#' @param pairs \code{\link{data.table}} with pairs. Should contain the columns 
#'   \code{.x} and \code{.y}.
#' @param variable character vector with name of resulting column name that is added
#'   to pairs.
#' @param x_vars character vector with the column names from \code{x} on which to 
#'   compare. 
#' @param y_vars character vector with the column names from \code{y} on which to 
#'   compare. 
#' @param fun function with which the variables are compared. When \code{x_vars} and 
#'   \code{y_vars} have length 1, this function should accept two vectors. Otherwise
#'   it will receive two \code{data.tables}. Function should either return a vector
#'   or a \code{data.table} with multiple columns.
#' @param x \code{data.table} with one half of the pairs.
#' @param y \code{data.table} with the other half of the pairs.
#' @param inplace logical indicating whether \code{pairs} should be modified in place. When
#'   pairs is large this can be more efficient.
#'
#' @details
#' When \code{fun} returns a \code{data.table} multiple columns are added to \code{pairs}. 
#' The names of these columns are \code{variable} pasted together with the names of 
#' the \code{data.table} returned by \code{fun} (separated by "_"). 
#' 
#' @return
#' Returns the \code{data.table} \code{pairs} with one or more columns added. 
#' 
#' @export
compare_vars <- function(pairs, variable, x_vars = variable, y_vars = x_vars, fun = identical(), 
    x = attr(pairs, 'x'), y = attr(pairs, 'y'), inplace = FALSE) {
  xv <- x[pairs$.x, ..x_vars]
  yv <- y[pairs$.y, ..y_vars]
  # Compare
  res <- if (ncol(xv) == 1 && ncol(yv) == 1) 
    fun(xv[[1]], yv[[1]]) else fun(xv, yv)
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

