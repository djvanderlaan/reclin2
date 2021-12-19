
#' @importFrom parallel clusterCall
#' @export
compare_pairs.cluster_pairs <- function(pairs, on, 
    comparators = list(default_comparator), default_comparator = identical(), 
    new_name = NULL, ...) {
  
  tmp <- clusterCall(pairs$cluster, function(name, on, comparators, 
      default_comparator, new_name) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    env <- reclin_env[[name]]
    if (!is.null(new_name)) {
      reclin_env[[new_name]] <- new.env()
      new_env <- reclin_env[[new_name]]
      new_env$pairs <- compare_pairs(env$pairs, on = on, comparators = comparators, 
        default_comparator = default_comparator)
    } else {
      compare_pairs(env$pairs, on = on, comparators = comparators, 
        default_comparator = default_comparator, inplace = TRUE)
    }
  }, name = pairs$name, on = on, comparators = comparators, 
    default_comparator = default_comparator, new_name = new_name)
  if (!missing(new_name) && !is.null(new_name)) {
    pairs$name <- new_name
    pairs
  } else {
    invisible(pairs)
  }
}

