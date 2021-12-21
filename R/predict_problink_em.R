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
#' @param add add the predictions to the original pairs object
#' @param comparators a list of comparison functions (see \code{\link{compare_pairs}}). 
#'   When missing \code{attr(pairs, 'comparators')} is used. 
#' @param ... unused.
#'   
#' @return 
#' Returns a data.table with either the \code{.x} and \code{.y} columns from 
#' \code{pairs} (when \code{add = FALSE}) or all columns of \code{pairs}. To these 
#' columns are added: 
#'
#' \itemize{
#' \item In case of \code{type = "weights"} a column \code{weights} with the calculated
#'   weights.
#' \item In case of \code{type = "mpost"} a column \code{mpost} with the calculated
#'   posterior probabilities (probability that pair is a match given comparison vector.
#' \item In case of \code{type = "prob"} the columns \code{mprob} and \code{uprob} with the
#'   m and u-probabilites and \code{mpost} and \code{upost} with the posterior m- and
#'   u-probabilities.
#' \item In case of \code{type = "all"} all of the above.
#' }
#' 
#' @export
predict.problink_em <- function(object, pairs = newdata, newdata = NULL, 
    type = c("weights", "mpost", "probs", "all"), binary = FALSE, 
    add = FALSE, comparators, ...) {
  # Process input
  type <- match.arg(type)
  if (is.null(pairs)) pairs <- newdata
  if (is.null(pairs)) stop("Missing pairs or newdata.")
  if (missing(comparators) || is.null(comparators))  
    comparators <- get_comparators(pairs)
  # Initialise end result and for-loop
  predict_problinkem(pairs, object, type, binary, add, comparators) 
}



predict_problinkem <- function(pairs, model, type, binary, add, comparators, ...) {
  UseMethod("predict_problinkem")
}

#' @import data.table
predict_problinkem.pairs <- function(pairs, model, type, binary, add, comparators, ...) {
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
  res <- if (add) pairs else pairs[, list(.x, .y)]
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

