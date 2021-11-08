library(devtools)
load_all()



source("work/random_data.R")

n <- 5000
dta <- random_data(n1 = n, n2 = n*0.8, overlap = 0.2, perr = 0.1)

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

# Add the predictions to the original pairs
pairs <- predict(m, pairs = pairs, type = "all", add = TRUE)


# =============================================================================
# Example using machine learning

compare_vars(pairs, "truth", on_x = "id", on_y = "id", inplace = TRUE)

f <- formula(truth ~ last_name + street + number + first_name + dob_dy)
m <- glm(f, family = binomial(), data = pairs)

pairs[, prob := predict(m, type = "response")]


# =============================================================================
# Select pairs for linkage

select_threshold(pairs, "selected", "mpost", 0.0001, inplace = TRUE)

select_greedy(pairs, "selected_greedy", "weight", preselect = "selected", 
  inplace = TRUE)

table(pairs$selected, pairs$selected_greedy)

select_n_to_m(pairs, "selected_ntom", "weight", preselect = "selected", 
  inplace = TRUE)

table(pairs$selected, pairs$selected_ntom)

select_threshold(pairs, "selected_model", "prob", 0.5, inplace = TRUE)
select_n_to_m(pairs, "selected_ntom_model", "prob", preselect = "selected_model", 
  inplace = TRUE)

table(pairs$selected_ntom, pairs$selected_ntom_model)

# =============================================================================
# Create final linked dataset

res <- link(pairs, selection = "selected_ntom", x = x, y = y, 
  all_x = TRUE, all_y = FALSE, keep_from_pairs = "weight")

