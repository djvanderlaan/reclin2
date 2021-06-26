
# Used internally by `select_greedy` and `select_n_to_m`
#
select_preprocess <- function(pairs, score, threshold = NULL, preselect = NULL, 
    id_x = NULL, id_y = NULL, x = attr(pairs, 'x'), y = attr(pairs, 'y')) {
  
  if (is.character(score)) {
    stopifnot(score %in% names(pairs))
    score <- pairs[[score]]
  }
  # Proces selection: threshold/preselect
  select <- !logical(nrow(pairs))
  if (!missing(preselect) && !is.null(preselect)) {
    select <- if (is.character(preselect)) pairs[[preselect]] else preselect
    if (is.null(select)) stop("'", preselect, "' not found in pairs.")
  } 
  if (!missing(threshold) && !is.null(threshold)) {
    select <- select & (score > threshold)
  }
  # When id_x and id_y are not given it is assumed that every row in x and y are
  # unique elements; when given look for object identifier in resp x and y
  if (!is.null(id_x) && !missing(id_x)) {
    if (is.character(id_x)) id_x <- x[[id_x]]
  } else id_x <- pairs$.x
  if (!is.null(id_y) && !missing(id_y)) {
    if (is.character(id_y)) id_y <- y[[id_y]]
  } else id_y <- pairs$.y
  # Select possible matches
  data.table(
    .x = id_x[select],
    .y = id_y[select],
    score = score[select],
    index = which(select)
  )
}

