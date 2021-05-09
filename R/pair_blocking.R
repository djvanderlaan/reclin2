

#' @import data.table
#' @export
pair_blocking <- function(x, y, on, add_xy = TRUE) {
  x <- as.data.table(x)
  y <- as.data.table(y)
  a <- x[, ..on]
  a$.x <- seq_len(nrow(a))
  b <- y[, ..on]
  b$.y <- seq_len(nrow(b))
  pairs <- merge(a, b, by = on, all.x = FALSE, all.y = FALSE, 
    allow.cartesian = TRUE)
  pairs <- pairs[, c(".x", ".y")]
  pairs <- structure(pairs, blocking_on = on)
  class(pairs) <- c("pairs", class(pairs))
  if (add_xy) {
    attr(pairs, "x") <- x
    attr(pairs, "y") <- y
  }
  pairs
}

