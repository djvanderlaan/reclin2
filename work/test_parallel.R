library(devtools)
load_all()



source("work/random_data.R")

n <- 5000
dta <- random_data(n1 = n, n2 = n*0.8, overlap = 0.2, perr = 0.2)

x <- as.data.table(dta[[1]])
y <- as.data.table(dta[[2]])


library(parallel)
cl <- makeCluster(4)

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

system.time({
  clpairs <- cluster_pair(cl, x, y)
})

system.time({
  clpairs <- cluster_pair_minsim(cl, x, y, on = names(x)[1:8], minsim = 2)
})

system.time({
  clpairs <- cluster_pair_blocking(cl, x, y, on = "postcode")
})

# =============================================================================
# Compare pairs on variables 

vars <- c("last_name", "street", "number", "first_name", "dob_yr", 
  "dob_mo", "dob_dy")

compare_pairs(pairs, vars, inplace = TRUE, 
  comparators = list(last_name = jaro_winkler(0.85) ))

compare_pairs(clpairs, vars,  
  comparators = list(last_name = jaro_winkler(0.85) ))

# =============================================================================
# Estimate EM-model and calculate weights

tab <- tabulate_patterns(pairs, on = vars)
cltab <- tabulate_patterns(clpairs, on = vars)

f <- formula(~ last_name + street + number + first_name + dob_dy)

m <- problink_em(f, patterns = tab)
summary(m)

clm <- problink_em(f, patterns = cltab)
summary(clm)


f <- as.formula(paste0("~", paste0(vars, collapse = "+")))
m <- problink_em(f, data = pairs)
clm <- problink_em(f, data = clpairs)

# Add the predictions to the original pairs
pairs <- predict(m, pairs = pairs, type = "all", add = TRUE)

tmp <- predict(clm, pairs = clpairs, new_name = "foo", add = TRUE)
clpairs <- predict(clm, pairs = clpairs, type = "all", add = TRUE)


# =============================================================================
# Select pairs for linkage

select_threshold(pairs, "selected", "mpost", 0.0001, inplace = TRUE)
select_threshold(clpairs, "selected", "mpost", 0.0001, inplace = TRUE)

select_greedy(pairs, "selected_greedy", "weight", preselect = "selected", 
  inplace = TRUE)

table(pairs$selected, pairs$selected_greedy)

select_n_to_m(pairs, "selected_ntom", "weight", preselect = "selected", 
  inplace = TRUE)

table(pairs$selected, pairs$selected_ntom)



# =============================================================================
# Create final linked dataset

res <- link(pairs, selection = "selected_ntom", x = x, y = y, 
  all_x = TRUE, all_y = FALSE, keep_from_pairs = "weight")










library(devtools)
load_all()

source("work/random_data.R")

n <- 5000
dta <- random_data(n1 = n, n2 = n*0.8, overlap = 0.2, perr = 0.2)

x <- as.data.table(dta[[1]])
y <- as.data.table(dta[[2]])


library(parallel)
cl <- makeCluster(4)

system.time({
  clpairs <- cluster_pair_blocking(cl, x, y, on = "postcode")
})

vars <- c("last_name", "street", "number", "first_name", "dob_yr", 
  "dob_mo", "dob_dy")

compare_pairs(clpairs, vars,  
  comparators = list(last_name = jaro_winkler(0.85) ))

f <- as.formula(paste0("~", paste0(vars, collapse = "+")))
clm <- problink_em(f, data = clpairs)

# Add the predictions to the original pairs
clpairs <- predict(clm, pairs = clpairs, type = "all", add = TRUE)


compare_vars(clpairs, variable = "id")



cluster_call(clpairs, function(pairs, ...) {
  sel1 <- sample(which(pairs$id), 100)
  sel2 <- sample(which(!pairs$id), 100)
  pairs[, sample := FALSE]
  pairs[c(sel1, sel2), sample := TRUE]
  NULL
})

sample <- cluster_collect(clpairs, "sample")

m <- glm(id ~ last_name + first_name, data = sample)


cluster_modify_pairs(clpairs, function(pairs, model, ...) {
  pairs$pmodel <- predict(model, newdata = pairs, type = "response")
  pairs
}, model = m)

cluster_call(clpairs, function(pairs, model, ...) {
  pairs[, pmodel := predict(model, newdata = pairs, type = "response")]
  NULL
}, model = m)



