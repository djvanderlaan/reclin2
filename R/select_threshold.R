

#' @export
#' 
select_threshold <- function(pairs, variable, score, threshold, 
    inplace = FALSE, ...) {
  ..score <- ..threshold <- NULL # To suppress R CMD check notes
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
