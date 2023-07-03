
#' @rdname select_unique
#' @export
select_unique.cluster_pairs <- function(pairs, variable, preselect = NULL, n = 1, m = 1,
    id_x = NULL, id_y = NULL, ...) {
  stop("select_unique is not implemented for cluster_pairs. First use select_threshold to ",
    "select a subset of the records. Then call cluster_collect with the selection to ",
    "get the relevant pairs locally. Then call select_unique on those pairs.")
}

