

#' Greedy one-to-one matching of pairs
#' 
#' @param x id's of lhs of pairs; converted to integer
#' @param y id's of rhs of pairs; converted to integer
#' @param weight numeric vector with weight of pair
#' @param n an integer. Each element of x can be linked to at most n elements of
#'   y. 
#' @param m an integer. Each element of y can be linked to at most m elements of
#'   x. 
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
greedy <- function(x, y, weight, n = 1L, m = 1L, include_ties = FALSE) {
  stopifnot(length(x) == length(y))
  stopifnot(length(x) == length(weight))
  if (anyNA(weight)) stop("Missing values in weight.")
  stopifnot(length(n) == 1 && is.numeric(n) && !is.na(n) && n > 0)
  stopifnot(length(m) == 1 && is.numeric(m) && !is.na(m) && m > 0)
  if (include_ties && (n > 1 || m > 1))
    stop("n>1 or m>1 can only be used when include_ties = FALSE")
  o <- order(weight, decreasing = TRUE)
  x <- x[o]
  y <- y[o]
  weight <- weight[o]
  if (include_ties) {
    s <- greedy_rcpp(x, y, weight, include_ties)
  } else {
    s <- greedy_nm_rcpp(x, y, weight, n, m)
  }
  s[o] <- s
  s
}

