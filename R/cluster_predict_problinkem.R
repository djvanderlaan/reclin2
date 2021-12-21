

#' @importFrom parallel clusterCall
predict_problinkem.cluster_pairs <- function(pairs, model, type, binary, add, 
    comparators, new_name = NULL, ...) {
  if (add == FALSE) 
    stop("add = FALSE is not supported for cluster_pairs. The predictions ",
      "need to be added to pairs. If you don't want to add the predictions ",
      "to the current pairs, you can use the new_name argument to create a ",
      "new set.")
  tmp <- clusterCall(pairs$cluster, function(name, model, type, binary, 
      comparators, new_name) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    env <- reclin_env[[name]]
    pairs <- env$pairs
    if (!is.null(new_name)) {
      reclin_env[[new_name]] <- new.env()
      env <- reclin_env[[new_name]]
    }
    p <- predict(model, newdata = pairs, type = type, binary = binary, 
      add = TRUE, comparators = comparators)
  }, name = pairs$name, model = model, type = type, binary = binary, 
    comparators = comparators, new_name = new_name)
  if (!missing(new_name) && !is.null(new_name)) pairs$name <- new_name
  pairs
}

