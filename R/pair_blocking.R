#' Generate pairs using simple blocking
#'
#' Generates all combinations of records from \code{x} and \code{y} where the 
#' blocking variables are equal. 
#'
#' @param x first \code{data.frame}
#' @param y second \code{data.frame}
#' @param on the variables defining the blocks or strata for which 
#'   all pairs of \code{x} and \code{y} will be generated.
#' @param add_xy add \code{x} and \code{y} as attributes to the returned 
#'   pairs. This makes calling some subsequent operations that need \code{x} and 
#'   \code{y} (such as \code{\link{compare_pairs}} easier.
#'
#' @details
#' Generating (all) pairs of the records of two data sets, is usually the first 
#' step when linking the two data sets. However, this often results in a too 
#' large number of records. Therefore, blocking is usually applied. 
#'  
#' @return 
#' A \code{\link{data.table}} with two columns, 
#' \code{.x} and \code{.y}, is returned. Columns \code{.x} and \code{.y} are 
#' row numbers from \code{data.frame}s \code{.x} and \code{.y} respectively. 
#'
#' @seealso
#' \code{\link{pair}} and \code{\link{pair_minsim}} are other methods
#' to generate pairs. 
#'
#' @examples
#' data("linkexample1", "linkexample2")
#' pairs <- pair_blocking(linkexample1, linkexample2, "postcode")
#'
#' @import data.table
#' @export
pair_blocking <- function(x, y, on, add_xy = TRUE) {
  x <- as.data.table(x)
  y <- as.data.table(y)
  a <- x[, ..on]
  a[, .x := seq_len(nrow(a))]
  b <- y[, ..on]
  b[, .y := seq_len(nrow(b))]
  pairs <- merge(a, b, by = on, all.x = FALSE, all.y = FALSE, 
    allow.cartesian = TRUE)
  pairs[, (on) := NULL]
  setattr(pairs, "class", c("pairs", class(pairs)))
  setattr(pairs, "blocking_on", on)
  if (add_xy) {
    setattr(pairs, "x", x)
    setattr(pairs, "y", y)
  }
  pairs
}

