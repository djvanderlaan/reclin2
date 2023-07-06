

#' @rdname compare_vars
#' @export
compare_vars.cluster_pairs <- function(pairs, variable, on_x = variable, on_y = on_x, 
    comparator = cmp_identical(), new_name = NULL, ...) {
  tmp <- clusterCall(pairs$cluster, function(name, variable, on_x, on_y, comparator, 
      new_name) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    env <- reclin_env[[name]]
    if (!is.null(new_name)) {
      reclin_env[[new_name]] <- new.env()
      new_env <- reclin_env[[new_name]]
      new_env$pairs <- compare_vars(env$pairs, variable = variable, 
        on_x = on_x, on_y = on_y, comparator = comparator, inplace = FALSE)
    } else {
      if (exists(variable, env$pairs)) env$pairs[, (variable) := NULL]
      compare_vars(env$pairs, variable = variable, on_x = on_x, on_y = on_y, 
        comparator = comparator, inplace = TRUE)
    }
  }, name = pairs$name, variable = variable, on_x = on_x, on_y = on_y, 
    comparator = comparator, new_name = new_name)
  if (!missing(new_name) && !is.null(new_name)) {
    pairs$name <- new_name
    pairs
  } else {
    invisible(pairs)
  }
}

