

#' @export
#' 
select_threshold <- function(pairs, variable, score, threshold, 
    inplace = FALSE, ...) {
  if (is.character(score)) {
    stopifnot(score %in% names(pairs))
    score <- pairs[[score]]
  }
  if (inplace) {
    pairs[, (variable) := ..score > ..threshold]
  } else {
    pairs[[variable]] <- score > threshold
  }
  if (inplace) invisible(pairs) else pairs
}
