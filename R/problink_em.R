
#' Calculate EM-estimates of m- and u-probabilities
#'
#' @param formula a formula object with the variables for which to calculate the
#'   m-  and u-probabilities. Should be of the form \code{~ var1 + var2}. 
#' @param data data set with pairs on which to estimate the model. Alternatively
#'   one can use the \code{patterns} argument.
#' @param patterns table of patterns (as output by 
#'   \code{\link{tabulate_patterns}}).
#' @param mprobs0,uprobs0 initial values of the m- and u-probabilities. These
#'   should be lists with numeric values. The names of the elements in the list
#'   should correspond to the names in \code{by_x} in \code{\link{compare_pairs}}. 
#' @param p0 the initial estimate of the probability that a pair is a match.
#' @param tol when the change in the m and u-probabilities is smaller than \code{tol}
#'   the algorithm is stopped. 
#' @param mprob_max maximum values of the estimated m-probabilities. Values equal to
#'   one can lead to numerical instabilities.
#' @param uprob_min maximum values of the estimated m-probabilities. Values equal to
#'   zero can lead to numerical instabilities.
#'   
#' @return 
#' Returns an object of type \code{problink_em}. This is a list containing the
#' estimated \code{mprobs}, \code{uprobs} and overall linkage probability 
#' \code{p}. It also contains the table of comparison \code{patterns}. 
#' 
#' @references 
#' Fellegi, I. and A. Sunter (1969). "A Theory for Record Linkage", 
#' \emph{Journal of the American Statistical Association}. 64 (328): 
#' pp. 1183-1210. doi:10.2307/2286061.
#' 
#' Herzog, T.N., F.J. Scheuren and W.E. Winkler (2007). 
#' \emph{Data Quality and Record Linkage Techniques}, Springer.
#' 
#' @examples 
#' data("linkexample1", "linkexample2")
#' pairs <- pair_blocking(linkexample1, linkexample2, "postcode")
#' pairs <- compare_pairs(pairs, c("lastname", "firstname", "address", "sex"))
#' model <- problink_em(~ lastname + firstname + address + sex, data = pairs)
#' summary(model)
#'
#' @importFrom stats as.formula
#' @importFrom utils head
#' @export
problink_em <- function(formula, data, patterns, mprobs0 = list(0.95), 
    uprobs0 = list(0.02), p0 = 0.05, tol = 1E-5, mprob_max = 0.999, 
    uprob_min = 0.0001) {
  if (missing(formula) && !missing(patterns)) {
    formula = as.formula(paste0("~", paste0(head(names(patterns), -1), 
      collapse ="+")))
  }
  by <- if (length(formula) == 3) all.vars(formula[[3]]) else 
    all.vars(formula)
  if (!missing(data)) {
    stopifnot(all(by %in% names(data)))
    patterns <- tabulate_patterns(data, on = by, complete = TRUE)
  } 
  stopifnot(all(by %in% names(patterns)))
  # check and process input
  mprobs0 <- extend_to(by, mprobs0, 0.95)
  uprobs0 <- extend_to(by, uprobs0, 0.05)
  if (!is.numeric(p0) || length(p0) > 1 || p0 < 0 || p0 > 1)
    stop("p0 should be a number between 0 and 1.")
  # initialisation
  mprobs <- mprobs_prev <- mprobs0
  uprobs <- uprobs_prev <- uprobs0
  p      <- p_prev      <- p0
  recalculate_p_and_stop <- FALSE
  while (TRUE) {
    # estep
    a <- rep(1, nrow(patterns))
    b <- rep(1, nrow(patterns))
    for (col in by) {
      m     <- patterns[[col]]
      a     <- a * ifelse(m, mprobs[[col]], 1-mprobs[[col]])
      b     <- b * ifelse(m, uprobs[[col]], 1-uprobs[[col]])
    }
    gm <- p*a / (p*a + (1-p)*b)
    gu <- p*b / (p*a + (1-p)*b)
    # mstep
    p <- sum(patterns$n*gm)/sum(patterns$n)
    if (recalculate_p_and_stop) break;
    for (col in by) {
      m             <- patterns[[col]]
      mprobs[[col]] <- sum(patterns$n*gm*m) / sum(patterns$n*gm)
      if (mprobs[[col]] > mprob_max) mprobs[[col]] <- mprob_max
      uprobs[[col]] <- sum(patterns$n*gu*m) / sum(patterns$n*gu)
      if (uprobs[[col]] < uprob_min) uprobs[[col]] <- uprob_min
    }
    # check convergence
    eps <- 0
    for (col in by) {
      eps <- eps + sum((mprobs[[col]] - mprobs_prev[[col]])^2)
      if (eps > tol) break
      eps <- eps + sum((uprobs[[col]] - uprobs_prev[[col]])^2)
      if (eps > tol) break
    }
    if (eps < tol) recalculate_p_and_stop <- TRUE
    mprobs_prev <- mprobs
    uprobs_prev <- uprobs
  }
  structure(list(mprobs=mprobs, uprobs=uprobs, p=p, patterns=patterns), 
    class="problink_em")
}


#' Summarise the results from \code{\link{problink_em}}
#' 
#' @param object the \code{\link{problink_em}} object.
#' @param ... ignored;
#' 
#' @export
summary.problink_em <- function(object, ...) {
  # calculate the posterior probabilities
  m_prob <- rep(1, nrow(object$patterns))
  u_prob <- rep(1, nrow(object$patterns))
  w <- rep(0, nrow(object$patterns))
  for (col in names(object$mprobs)) {
    m <- object$patterns[[col]]
    pm <- (1 - object$mprobs[[col]]) + (2*object$mprobs[[col]]-1)*m
    pu <- (1 - object$uprobs[[col]]) + (2*object$uprobs[[col]]-1)*m
    w <- w + log(pm/pu)
    m_prob <- m_prob * ifelse(m, object$mprobs[[col]], 1-object$mprobs[[col]])
    u_prob <- u_prob * ifelse(m, object$uprobs[[col]], 1-object$uprobs[[col]])
  }
  m_post <- object$p*m_prob / (object$p*m_prob + (1-object$p)*u_prob)
  u_post <- (1-object$p)*u_prob / (object$p*m_prob + (1-object$p)*u_prob)
  object$patterns$m_prob <- m_prob
  object$patterns$u_prob <- u_prob
  object$patterns$m_post <- m_post
  object$patterns$u_post <- u_post
  object$patterns$weight <- log(m_prob) - log(u_prob)
  object$patterns$weight <- w
  # return orignal model with additional stats
  structure(object, class="summary_problink_em")
}


#' @export
print.summary_problink_em <- function(x, ...) {
  cat("M- and u-probabilities estimated by the EM-algorithm:\n")
  tab <- data.frame(variable = names(x$mprobs), mprobs = unlist(x$mprobs), 
    uprobs = unlist(x$uprobs))
  names(tab) <- c("Variable", "M-probability", "U-probability")
  print(tab, row.names=FALSE)
  cat("\nMatching probability: ", x$p, ".\n", sep="")
  cat("\nPatterns:\n")
  o <- order(x$patterns$m_post, decreasing = TRUE)
  x$patterns <- x$patterns[o,]
  for (col in c("m_prob", "u_prob", "m_post", "u_post", "weight")) {
    x$patterns[[col]] <- round(x$patterns[[col]], 3)
  }
  print(x$patterns, row.names=FALSE)
}

#' @export
print.problink_em <- function(x, ...) {
  cat("M- and u-probabilities estimated by the EM-algorithm:\n")
  tab <- data.frame(variable = names(x$mprobs), mprobs = unlist(x$mprobs), 
    uprobs = unlist(x$uprobs))
  names(tab) <- c("Variable", "M-probability", "U-probability")
  print(tab, row.names=FALSE)
  cat("\nMatching probability: ", x$p, ".\n", sep="")  
}
