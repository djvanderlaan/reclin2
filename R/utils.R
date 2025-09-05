
#' Return default value if value is missing or NULL
#'
#' @param val value to check
#'
#' @param or value to return if \code{val} is missing or \code{NULL}. 
#'
#' @return
#' Returns \code{or} is val is \code{NULL}. Otherwise it returns
#' \code{ifelse(is.na(val), or, val)}.
#'
#' @export
val_or <- function(val, or) {
  if (is.null(val)) {
    or
  } else {
    ifelse(is.na(val), or, val)
  }
}

