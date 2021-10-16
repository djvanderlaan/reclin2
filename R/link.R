
#' @export
#' 
link <- function(pairs, selection = NULL, all = FALSE, all_x = all, all_y = all, 
    x = attr(pairs, "x"), y = attr(pairs, "y"), suffixes = c(".x", ".y"),
    keep_from_pairs = c(".x", ".y")) {
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
  if (!(".x" %in% keep_from_pairs)) res[, .x := NULL]
  if (!(".y" %in% keep_from_pairs)) res[, .y := NULL]
  res
}

