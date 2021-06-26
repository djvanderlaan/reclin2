library(devtools)
load_all()



source("work/random_data.R")

n <- 5000
dta <- random_data(n1 = n, n2 = n*0.8, overlap = 0.2)

x <- as.data.table(dta[[1]])
y <- as.data.table(dta[[2]])


# =============================================================================
# Different methods for generating pairs

system.time({
  pairs <- pair(x, y)
})
nrow(pairs)

system.time({
  pairs <- pair_minsim(x, y, on = names(x)[1:8], minsim = 2)
})
nrow(pairs)

system.time({
  pairs <- pair_blocking(x, y, on = "postcode")
})
nrow(pairs)

# =============================================================================
# Compare pairs on variables


vars <- c("last_name", "street", "number", "first_name", "dob_yr", 
  "dob_mo", "dob_dy")


compare_pairs(pairs, vars, inplace = TRUE, 
  comparators = list(last_name = jaro_winkler(0.85) ))


# =============================================================================
# Estimate EM-model and calculate weights

tab <- tabulate_patterns(pairs, on = vars)


f <- formula(~ last_name + street + number + first_name + dob_dy)

m <- problink_em(f, patterns = tab)
summary(m)

f <- as.formula(paste0("~", paste0(vars, collapse = "+")))
m <- problink_em(f, data = pairs)

p <- predict(m, pairs = pairs, type = "all")
# Add the predictions to the original pairs
# TODO: do we need to put this functionality in predict as this is something 
# that will be commonly done?
pairs <- pairs[p, on = c(".x", ".y")]




# Select pairs for linkage ------------------------------------------------

select_threshold(pairs, "selected", "mpost", 0.0001, inplace = TRUE)

select_greedy(pairs, "selected_greedy", "weight", preselect = "selected", 
  inplace = TRUE)

table(pairs$selected, pairs$selected_greedy)

select_n_to_m(pairs, "selected_ntom", "weight", preselect = "selected", 
  inplace = TRUE)

table(pairs$selected, pairs$selected_ntom)




link <- function(pairs, selection = NULL, all_x = TRUE, all_y = TRUE, 
    x = attr(pairs, "x"), y = attr(pairs, "y"), suffixes = c(".x", ".y"),
    keep_from_pairs = NULL) {
  x$.x <- seq_len(nrow(x))
  y$.y <- seq_len(nrow(y))
  if (!missing(selection) && !is.null(selection)) {
    if (is.character(selection)) {
      stopifnot(selection %in% names(pairs))
      selection <- pairs[[selection]]
    }
  } else selection <- TRUE
  vars <- unique(c(".x", ".y", keep_from_pairs))
  res <- merge(pairs[selection == TRUE, ..vars], x, all.x = TRUE, 
    all.y = all_x, by = ".x", suffixes = c("_pairs", ""))
  res <- merge(res, y, all.x = TRUE, all.y = all_y, by = ".y",
    suffixes = suffixes)
  res
}


res <- link(pairs, selection = "selected_ntom", x = x, y = y, all_x = TRUE, all_y = FALSE)

table(is.na(res$id.x), is.na(res$id.y))

nrow(x)

x
x[, .x := .I]
y[, .y := .I]

?.N





link_impl <- function(pairs, selection = NULL, x = NULL, y = NULL, 
    all_x = TRUE, all_y = TRUE) {
  # Process x and y
  if (missing(x) || is.null(x)) x <- attr(pairs, "x")
  if (is.null(x)) stop("Missing x")
  if (missing(y) || is.null(y)) y <- attr(pairs, "y")
  if (is.null(y)) stop("Missing y")
  x$.x <- seq_len(nrow(x))
  y$.y <- seq_len(nrow(y))
  # Process selection
  if (missing(selection) || is.null(selection)) 
    selection <- attr(pairs, "selection")
  # (repeat previous if.. in case attribute was not set)
  if (missing(selection) || is.null(selection)) {
    selection <- TRUE
  } else if (is.character(selection)) {
    if (length(selection) != 1) 
      stop("When selection is a character vector; it needs to be length 1.")
    selection <- pairs[[selection]]
  } 
  # Link
  res <- data.frame('.x' = as_rvec(pairs$x[selection]),
    '.y' = as_rvec(pairs$y[selection]))
  res <- if (all_x) dplyr::full_join(res, x, by = ".x") else
    dplyr::left_join(res, x, by = ".x")
  res <- if (all_y) dplyr::full_join(res, y, by = ".y") else
    dplyr::left_join(res, y, by = ".y")
  res$.x <- NULL
  res$.y <- NULL
  res
  # The following gnerates note in R CMD check
  # dplyr::select(res, -`.x`, -`.y`)
}



