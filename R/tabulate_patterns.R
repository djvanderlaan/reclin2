
#' @import data.table
#' @export
tabulate_patterns <- function(pairs, on, comparators, complete = TRUE, ...) {
  # Process arguments
  if (missing(comparators) || is.null(comparators))  {
    # when using compare_vars or compare_pairs, the comparator is stored
    # as an attribute in the column; retreive those
    comparators <- lapply(pairs, attr, which = "comparator")
    # remove elements with comparator
    comparators <- comparators[!sapply(comparators, is.null)]
  }
  if (missing(on) || is.null(on)) on <- names(comparators)
  # Tabulate
  for (var in on) {
    cmp_fun <- comparators[[var]]
    if (!is.null(cmp_fun))
      pairs[[var]] <- cmp_fun(pairs[[var]])
    # we don't use the := assignment from data.table because we don't
    # want to modify the original dataset
  }
  tab <- pairs[, .(n = .N), by = on]
  # Add patterns not present in dataset
  if (complete) {
    possible_patterns <- lapply(tab[, ..on], function(x) {
      u <- unique(x)
      if (is.factor(x)) union(x, levels(x)) else u
    })
    possible_patterns <- do.call(CJ, possible_patterns)
    tab <- tab[possible_patterns, , on = on]
    tab$n[is.na(tab$n)] <- 0
  }
  tab
}

