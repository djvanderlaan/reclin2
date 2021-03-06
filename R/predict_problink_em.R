
#' Calculate weights and probabilities for pairs
#' 
#' @param object an object of type \code{problink_em} as produced by 
#'   \code{\link{problink_em}}.
#' @param pairs a object with pairs for which to calculate weights.
#' @param newdata an alternative name for the \code{pairs} argument. Specify 
#'   \code{newdata} or \code{pairs}. 
#' @param type a character vector of length one specifying what to calculate. 
#'   See results for more information.
#' @param binary convert comparison vectors to binary vectors using the 
#'   comparison function in comparators. 
#' @param comparators a list of comparison functions (see \code{\link{compare_pairs}}). 
#'   When missing \code{attr(pairs, 'comparators')} is used. 
#' @param ... unused.
#'   
#' @return 
#' In case of \code{type == "weights"} returns a vector (\code{\link{lvec}} or
#' regular R-vector depending on the type of \code{pairs}). with the linkage weights. 
#' In case of \code{type == "mpost"} returns a vector with the posterior m-probabilities
#' (probability that a pair is a match). In case of \code{type == "probs"} returns a
#' data.frame or \code{\link{ldat}} with the m- and u-probabilities and posterior
#' m- and u probabilities. In case \code{type == "all"} returns a \code{data.frame} or 
#' \code{\link{ldat}} with both probabilities and weights. 
#' 
#' @export
predict.problink_em <- function(object, pairs = newdata, newdata = NULL, 
    type = c("weights", "mpost", "probs", "all"), binary = FALSE, 
    comparators, ...) {
  # Process input
  type <- match.arg(type)
  if (is.null(pairs)) pairs <- newdata
  if (is.null(pairs)) stop("Missing pairs or newdata.")
  if (missing(comparators)) comparators <- attr(pairs, "comparators")
  # Initialise end result and for-loop
  predict_problinkem(pairs, object, type, binary, comparators) 
}



predict_problinkem <- function(pairs, model, type, binary, comparators, ...) {
  UseMethod("predict_problinkem")
}

#' @import data.table
predict_problinkem.pairs <- function(pairs, model, type, binary, comparators, ...) {
  on <- names(model$mprobs)
  # Initialise end result and for-loop
  weights <- rep(0, nrow(pairs))
  mprobs  <- rep(1, nrow(pairs))
  uprobs  <- rep(1, nrow(pairs))
  # Calculate weight contribution of each variable
  for (col in on) {
    comp <- if (binary) comparators[[col]](pairs[[col]]) else pairs[[col]]
    pm <- (1 - model$mprobs[[col]]) +
              (2 * model$mprobs[[col]] - 1) * comp
    pu <- (1 - model$uprobs[[col]]) + 
              (2 * model$uprobs[[col]] - 1) * comp
    w  <- log(pm / pu)
    # Give pairs with missing values a weight 0 for corresponding variable
    w[is.na(w)] <- 0
    # Add weight, mprob, uprob to total vectors
    weights <- weights + w
    mprobs  <- mprobs * pm
    uprobs  <- uprobs * pu
  }
  # Construct end result
  res <- pairs[, .(.x, .y)]
  if (type == "weights") {
    res[, weights := weights]
  } else if (type == "mpost") {
    res[, mpost := mprobs * model$p / (mprobs * model$p + uprobs * (1 - model$p))]
  } else {
    res[, mprob := mprobs]
    res[, uprob := uprobs]
    res[, mpost := mprobs * model$p / (mprobs * model$p + uprobs * (1 - model$p))]
    res[, upost := 1 - mpost]
    if (type == "all") res[,  weight := weights]
  } 
  res
}


#' @importFrom parallel clusterCall
predict_problinkem.cluster_pairs <- function(pairs, model, type, binary, 
      comparators, new_name = NULL, ...) {
  
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
      comparators = comparators)
    # TODO
    # Example how compare_pairs handles new_name
    # env$pairs <- compare_pairs(env$pairs, on = on, comparators = comparators, 
    #   default_comparator = default_comparator, overwrite = overwrite)
  }, name = pairs$name, model = model, type = type, binary = binary, 
    comparators = comparators, new_name = new_name)

  if (!missing(new_name) && !is.null(new_name)) pairs$name <- new_name
  pairs
}

