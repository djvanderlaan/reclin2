
#' @export 
compare_pairs <- function(pairs, on, comparators = list(default_comparator), 
    default_comparator = identical(), x = attr(pairs, 'x'), y = attr(pairs, 'y'), 
    inplace = FALSE, ...) {
  if (missing(on) && !missing(on)) on <- names(comparators)
  if (missing(on) || is.null(on)) stop("on is missing.")
  if (!all(on %in% names(x)))
    stop("Not all variables in on are present in x.")
  if (!all(on %in% names(y))) 
    stop("Not all variables in on are present in y.")
  comparators <- extend_to(on, comparators, default = default_comparator)  
  # Compare
  if (!inplace) pairs <- copy(pairs)
  for (var in on) {
    compare_vars(pairs, var, fun = comparators[[var]], x = x, y = y, 
      inplace = TRUE)
  }
  ## TODO attr(pairs, "compare_on") <- on
  ## TODO attr(pairs, "comparators") <- comparators
  if (inplace) invisible(pairs) else pairs
}

# #' @export
# compare_pairs <- function(pairs, on, comparators = list(default_comparator), 
#     default_comparator = identical(), ...) {
#   UseMethod("compare_pairs", pairs)
# }
# 
# 
# #' @export
# compare_pairs.pairs <- function(pairs, on, 
#     comparators = list(default_comparator), default_comparator = identical(), 
#     overwrite = FALSE, ...) {
#   # Process and preparare input
#   x <- attr(pairs, "x")
#   y <- attr(pairs, "y")
#   if (missing(on) && !missing(on)) on <- names(comparators)
#   if (missing(on) || is.null(on)) stop("on is missing.")
#   if (!overwrite && any(on %in% names(pairs))) 
#     stop("Variable in on already present in pairs.")
#   if (!all(on %in% names(x)))
#     stop("Not all variables in on are present in x.")
#   if (!all(on %in% names(y))) 
#     stop("Not all variables in on are present in y.")
#   comparators <- extend_to(on, comparators, default = default_comparator)  
#   # Compare
#   for (var in on) {
#     cmp_fun <- comparators[[var]]
#     pairs[[var]] <- cmp_fun(x[pairs$.x, ..var][[1]], y[pairs$.y, ..var][[1]])
#   }
#   attr(pairs, "compare_on") <- on
#   attr(pairs, "comparators") <- comparators
#   pairs
# }
# 
