
#' Comparison functions
#' 
#' @param threshold threshold to use for the Jaro-Winkler string distance when
#'   creating a binary result.
#'   
#' @section Warning:
#' The functions \code{identical}, \code{jaro_winkler}, \code{lcs} and
#' \code{jaccard} are deprecated and will be removed in future versions of the
#' package. Instead use the functions \code{cmp_identical},
#' \code{cmp_jarowinkler}, \code{cmp_lcs} and \code{cmp_jaccard}. 
#'
#' @details 
#' A comparison function should accept two arguments: both vectors. When the 
#' function is called with both arguments it should compare the elements in the 
#' first vector to those in the second. When called in this way, both vectors
#' have the same length. What the function should return depends on the methods
#' used to score the pairs. Usually the comparison functions return a similarity
#' score with a value of 0 indication complete difference and a value > 0 
#' indicating similarity (often a value of 1 will indicate perfect similarity). 
#' 
#' Some methods, such as \code{\link{problink_em}}, can handle similarity
#' scores, but also need binary values (\code{0}/\code{FALSE} = complete 
#' dissimilarity; \code{1}/\code{TRUE} = complete similarity). In order to
#' allow for this the comparison function is called with one argument.
#' 
#' When the comparison is called with one argument, it is passed the result of
#' a previous comparison. The function should translate that result to a binary 
#' (\code{TRUE}/\code{FALSE} or \code{1}/\code{0}) result. The result should 
#' not contain missing values. 
#' 
#' The \code{jaro_winkler}, \code{lcs} and \code{jaccard} functions use the corresponding 
#' methods from \code{\link{stringdist}} except that they are transformed from
#' a distance to a similarity score.
#' 
#' @return 
#' The functions return a comparison function (see details).
#' 
#' @examples 
#' cmp <- cmp_identical()
#' x <- cmp(c("john", "mary", "susan", "jack"), 
#'          c("johan", "mary", "susanna", NA))
#' # Applying the comparison function to the result of the comparison results 
#' # in a logical result, with NA's and values of FALSE set to FALSE
#' cmp(x)
#' 
#' cmp <- cmp_jarowinkler(0.95)
#' x <- cmp(c("john", "mary", "susan", "jack"), 
#'          c("johan", "mary", "susanna", NA))
#' # Applying the comparison function to the result of the comparison results 
#' # in a logical result, with NA's and values below the threshold FALSE
#' cmp(x)
#' 
#' \dontshow{gc()}
#' 
#' @rdname comparators
#' @export
cmp_identical <- function() {
  function(x, y) {
    if (is.factor(x) || (!missing(y) && is.factor(y))) {
      x <- as.character(x)
      y <- as.character(y)
    }
    if (!missing(y)) {
      x == y
    } else {
      x & !is.na(x)
    }
  }
}

#' @rdname comparators
#' @importFrom stringdist stringdist
#' @export
cmp_jarowinkler <- function(threshold = 0.95) {
  function(x, y) {
    if (!missing(y)) {
      1-stringdist(x, y, method = "jw")
    } else {
      (x > threshold) & !is.na(x)
    }
  }
}

#' @rdname comparators
#' @export
jaro_winkler <- function(threshold = 0.80) {
  deprecated_warn(paste0("identical, jaro_winkler, lcs, jaccard",
      " are deprecated. Use the cmp_ variants (see ?cmp_identical).",
      " This warning is shown only once. Set the option",
      " reclin2_deprecate_warn to FALSE to disable these warnings."))
  cmp_jarowinkler(threshold = threshold)
}

#' @rdname comparators
#' @importFrom stringdist stringdist
#' @export
cmp_lcs <- function(threshold = 0.80) {
  function(x, y) {
    if (!missing(y)) {
      d <- stringdist(x, y, method = "lcs")
      maxd <- nchar(x) + nchar(y)
      1 - d/maxd
    } else {
      (x > threshold) & !is.na(x)
    }
  }
}

#' @rdname comparators
#' @export
lcs <- function(threshold = 0.80) {
  deprecated_warn(paste0("identical, jaro_winkler, lcs, jaccard",
      " are deprecated. Use the cmp_ variants (see ?cmp_identical).",
      " This warning is shown only once. Set the option",
      " reclin2_deprecate_warn to FALSE to disable these warnings."))
  cmp_lcs(threshold = threshold)
}


#' @rdname comparators
#' @importFrom stringdist stringdist
#' @export
cmp_jaccard <- function(threshold = 0.80) {
  function(x, y) {
    if (!missing(y)) {
      1-stringdist(x, y, method = "jaccard", q = 2)
    } else {
      (x > threshold) & !is.na(x)
    }
  }
}

#' @rdname comparators
#' @export
jaccard <- function(threshold = 0.80) {
  deprecated_warn(paste0("identical, jaro_winkler, lcs, jaccard",
      " are deprecated. Use the cmp_ variants (see ?cmp_identical).",
      " This warning is shown only once. Set the option",
      " reclin2_deprecate_warn to FALSE to disable these warnings."))
  cmp_jaccard(threshold = threshold)
}


deprecated_warn <- function(message, only_once = TRUE) {
  should_warn <- getOption("reclin2_deprecate_warn", TRUE)
  if (should_warn) {
    warning(message)
    if (only_once) options("reclin2_deprecate_warn" = FALSE)
  }
}
