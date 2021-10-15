

#' @import data.table
#' @export
pair_blocking <- function(x, y, on, add_xy = TRUE) {
  .x <- .y <- NULL # To suppress R CMD check notes
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

