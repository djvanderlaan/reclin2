#' Generate all possible pairs
#'
#' Generates all combinations of records from \code{x} and \code{y}.
#'
#' @param x first \code{data.frame}
#' @param y second \code{data.frame}. Ignored when \code{deduplication = TRUE}.
#' @param deduplication generate pairs from only \code{x}. Ignore \code{y}. This 
#'   is usefull for deduplication of \code{x}.
#' @param add_xy add \code{x} and \code{y} as attributes to the returned 
#'   pairs. This makes calling some subsequent operations that need \code{x} and 
#'   \code{y} (such as \code{\link{compare_pairs}} easier.
#'
#' @details
#' Generating (all) pairs of the records of two data sets, is usually the first 
#' step when linking the two data sets. 
#'  
#' @return 
#' A \code{\link{data.table}} with two columns, 
#' \code{.x} and \code{.y}, is returned. Columns \code{.x} and \code{.y} are 
#' row numbers from \code{data.frame}s \code{.x} and \code{.y} respectively. 
#'
#' @seealso
#' \code{\link{pair_blocking}} and \code{\link{pair_minsim}} are other methods
#' to generate pairs. 
#'
#' @examples
#' data("linkexample1", "linkexample2")
#' pairs <- pair(linkexample1, linkexample2)
#'
#' @import data.table
#' @export
pair <- function(x, y, deduplication = FALSE, add_xy = TRUE) {
  x <- as.data.table(x)
  if (deduplication && !missing(y)) warning("y provided will be ignored.")
  y <- if (deduplication) x else as.data.table(y)
  pairs <- CJ(.x = seq_len(nrow(x)), .y = seq_len(nrow(y)))
  # In case of deduplication; ignode cases when .y <= .x
  if (deduplication) pairs <- pairs[.y > .x]
  setattr(pairs, "class", c("pairs", class(pairs)))
  if (deduplication) setattr(pairs, "deduplication", TRUE)
  if (add_xy) {
    setattr(pairs, "x", x)
    setattr(pairs, "y", y)
  }
  pairs
}

