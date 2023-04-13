

#' Greedy one-to-one matching of pairs
#' 
#' @param x id's of lhs of pairs; converted to integer
#' @param y id's of rhs of pairs; converted to integer
#' @param weight numeric vector with weight of pair
#' @param include_ties when pairs for a given record have an equal weight, should
#'   all pairs be included.
#' 
#' @details 
#' Pairs with the highest weight are selected as long a neither the lhs as the
#' rhs are already selected in a pair with a higher weight. When \code{include_ties}
#' is \code{TRUE} all pairs are included when multiple pairs for a given record have
#' an equal weight.
#'
#' @return
#' A logical vector with the same length as \code{x}. 
#' 
#' @useDynLib reclin2
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' 
greedy <- function(x, y, weight, include_ties = FALSE) {
  stopifnot(length(x) == length(y))
  stopifnot(length(x) == length(weight))
  if (anyNA(weight)) stop("Missing values in weight.")
  o <- order(weight, decreasing = TRUE)
  x <- x[o]
  y <- y[o]
  weight <- weight[o]
  s <- greedy_rcpp(x, y, weight, include_ties)
  s[o] <- s
  s
}

