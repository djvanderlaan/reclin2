
#' @import data.table
#' @export
pair <- function(x, y, add_xy = TRUE) {
  x <- as.data.table(x)
  y <- as.data.table(y)
  pairs <- CJ(.x = seq_len(nrow(x)), .y = seq_len(nrow(y)))
  setattr(pairs, "class", c("pairs", class(pairs)))
  if (add_xy) {
    setattr(pairs, "x", x)
    setattr(pairs, "y", y)
  }
  pairs
}

