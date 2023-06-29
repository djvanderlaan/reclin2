
#' @rdname score_simple
#' @export
score_simple.cluster_pairs <- function(pairs, variable, on, w1 = 1.0, w0 = 0.0, wna = 0.0, 
    new_name = NULL, ...) {

  tmp <- clusterCall(pairs$cluster, function(name, variable, on, w1, w0, wna, new_name) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    env <- reclin_env[[name]]
    pairs <- env$pairs
    if (!is.null(new_name)) {
      reclin_env[[new_name]] <- new.env()
      env <- reclin_env[[new_name]]
      env$pairs <- score_simple(pairs, variable, on = on, w1 = w1, w0 = w0, 
        wna = wna, inplace = FALSE)
    } else {
      if (exists(variable, pairs)) pairs[, (variable) := NULL]
      score_simple(pairs, variable, on = on, w1 = w1, w0 = w0, 
        wna = wna, inplace = TRUE)
    }
  }, name = pairs$name, variable, on = on, w1 = w1, w0 = w0, 
      wna = wna, new_name = new_name)
  # Return
  if (!missing(new_name) && !is.null(new_name)) {
    pairs$name <- new_name
    pairs
  } else {
    invisible(pairs)
  }
}

