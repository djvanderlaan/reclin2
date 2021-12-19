
#' Add a variable from one of the data sets to pairs
#'
#' @param pairs \code{\link{data.table}} with pairs. Should contain the columns 
#'   \code{.x} and \code{.y}.
#' @param variable name of the variable that should be added
#' @param new_variable optional variable name of the new variable in
#'   \code{pairs}. When omitted \code{variable} is used.
#' @param ... other parameters are passed on to \code{compare_vars}. Especially
#'   \code{inplace}, \code{x} and \code{y} might be of interest.
#'
#' @return
#' Returns the pairs with the column added. When \code{inplace = TRUE}
#' \code{pairs} is returned invisibly and the original \code{pairs} is 
#' modified. 
#' 
#' @rdname add_from_x
#' @export
#' 
add_from_x <- function(pairs, variable, new_variable = variable, ...) {
  compare_vars(pairs, new_variable, on_x = variable, on_y = 1, 
    comparator = function(x,y) x, ...)
}

#' @rdname add_from_x
#' @export
#'
add_from_y <- function(pairs, variable, new_variable = variable, ...) {
  compare_vars(pairs, new_variable, on_y = variable, on_x = 1, 
    comparator = function(x,y) y, ...)
}

