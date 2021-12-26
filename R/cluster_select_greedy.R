#' @rdname select_n_to_m
#' @export
select_greedy.cluster_pairs <- function(pairs, variable, score, threshold, preselect = NULL, 
    id_x = NULL, id_y = NULL, ...) {
  stop("select_greedy is not implemented for cluster_pairs. First use select_threshold to ",
    "select a subset of the records. Then call cluster_collect with the selection to ",
    "get the relevant pairs locally. Then call select_greedy on those pairs.")
}
