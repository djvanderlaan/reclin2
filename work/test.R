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


