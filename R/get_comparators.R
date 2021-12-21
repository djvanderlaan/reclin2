
get_comparators <- function(pairs) {
  UseMethod("get_comparators")
}

get_comparators.pairs <- function(pairs) {
  # when using compare_vars or compare_pairs, the comparator is stored
  # as an attribute in the column; retreive those
  comparators <- lapply(pairs, attr, which = "comparator")
  # remove elements without comparator
  comparators[!sapply(comparators, is.null)]
}

