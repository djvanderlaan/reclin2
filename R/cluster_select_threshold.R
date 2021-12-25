
#' @rdname select_threshold
#' @export
select_threshold.cluster_pairs <- function(pairs, variable, score, threshold, 
    new_name = NULL, ...) {

  tmp <- clusterCall(pairs$cluster, function(name, variable, score, threshold, new_name) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    env <- reclin_env[[name]]
    pairs <- env$pairs
    if (!is.null(new_name)) {
      reclin_env[[new_name]] <- new.env()
      env <- reclin_env[[new_name]]
      env$pairs <- select_threshold(pairs, variable, score, threshold, 
        inplace = FALSE)
    } else {
      select_threshold(pairs, variable, score, threshold, inplace = TRUE)
    }
  }, name = pairs$name, variable, score, threshold, new_name)
  # Return
  if (!missing(new_name) && !is.null(new_name)) {
    pairs$name <- new_name
    pairs
  } else {
    invisible(pairs)
  }
}

