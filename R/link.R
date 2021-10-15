
#' @export
link <- function(pairs, selection = NULL, all_x = TRUE, all_y = TRUE, 
    x = attr(pairs, "x"), y = attr(pairs, "y"), suffixes = c(".x", ".y"),
    keep_from_pairs = NULL) {
  x$.x <- seq_len(nrow(x))
  y$.y <- seq_len(nrow(y))
  if (!missing(selection) && !is.null(selection)) {
    if (is.character(selection)) {
      stopifnot(selection %in% names(pairs))
      selection <- pairs[[selection]]
    }
  } else selection <- TRUE
  vars <- unique(c(".x", ".y", keep_from_pairs))
  res <- merge(pairs[selection == TRUE, ..vars], x, all.x = TRUE, 
    all.y = all_x, by = ".x", suffixes = c("_pairs", ""))
  res <- merge(res, y, all.x = TRUE, all.y = all_y, by = ".y",
    suffixes = suffixes)
  res
}

