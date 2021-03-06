
#' @importFrom parallel clusterCall
#' @export
compare_pairs.cluster_pairs <- function(pairs, on, 
    comparators = list(default_comparator), default_comparator = identical(), 
    overwrite = FALSE, new_name = NULL, ...) {
  
  tmp <- clusterCall(pairs$cluster, function(name, on, comparators, 
      default_comparator, overwrite, new_name) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    env <- reclin_env[[name]]
    pairs <- env$pairs
    if (!is.null(new_name)) {
      reclin_env[[new_name]] <- new.env()
      env <- reclin_env[[new_name]]
    }
    env$pairs <- compare_pairs(env$pairs, on = on, comparators = comparators, 
      default_comparator = default_comparator, overwrite = overwrite)
  }, name = pairs$name, on = on, comparators = comparators, 
    default_comparator = default_comparator, overwrite = overwrite,
    new_name = new_name)
  attr(pairs, "compare_on") <- on
  attr(pairs, "comparators") <- comparators
  if (!missing(new_name) && !is.null(new_name)) pairs$name <- new_name
  pairs
}
