
#' @import data.table
#' @export
pair <- function(x, y, add_xy = TRUE) {
  x <- as.data.table(x)
  y <- as.data.table(y)
  pairs <- CJ(.x = seq_len(nrow(x)), .y = seq_len(nrow(y)))
  class(pairs) <- c("pairs", class(pairs))
  if (add_xy) {
    attr(pairs, "x") <- x
    attr(pairs, "y") <- y
  }
  pairs
}

