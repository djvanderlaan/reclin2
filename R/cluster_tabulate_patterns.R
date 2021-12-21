

#' @importFrom parallel clusterCall
#' @import data.table
#' @export
tabulate_patterns.cluster_pairs <- function(pairs, on, comparators, 
    complete = TRUE, ...) {
  # Process arguments
#  if (missing(comparators) || is.null(comparators))  {
#    # when using compare_vars or compare_pairs, the comparator is stored
#    # as an attribute in the column; retreive those
#    comparators <- lapply(pairs, attr, which = "comparator")
#    # remove elements without comparator
#    comparators <- comparators[!sapply(comparators, is.null)]
#  }
#  if (missing(on) || is.null(on)) on <- names(comparators)
  if (missing(on)) on <- NULL
  if (missing(comparators)) comparators <- NULL
  # Run tabulate_pairs on all workers
  tabs <- clusterCall(pairs$cluster, function(name, on, comparators) {
    if (!require("reclin2"))
      stop("reclin2 needs to be installed on cluster nodes.")
    env <- reclin_env[[name]]
    pairs <- env$pairs
    tabulate_patterns(pairs, on = on, comparators = comparators, 
      complete = FALSE)
  }, name= pairs$name, on = on, comparators = comparators)
  # Combine results
  on  <- attr(tabs[[1]], "on")
  tab <- rbindlist(tabs)
  tab <- tab[, .(n = sum(n)), by = on]
  # Add patterns not present in dataset
  if (complete) {
    possible_patterns <- lapply(tab[, ..on], function(x) {
      u <- unique(x)
      if (is.logical(u)) u <- unique(c(u, c(TRUE, FALSE)))
      if (is.factor(x)) union(x, levels(x)) else u
    })
    possible_patterns <- do.call(CJ, possible_patterns)
    tab <- tab[possible_patterns, , on = on]
    tab$n[is.na(tab$n)] <- 0
  }
  tab
}

