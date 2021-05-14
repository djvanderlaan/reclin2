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


vars <- c("last_name", "postcode", "street", "number", "first_name", "dob_yr", 
  "dob_mo", "dob_dy")

for (var in vars) compare_vars(pairs, var, inplace = TRUE)

pairs <- compare_vars(pairs, "first_name", inplace = TRUE)

pairs <- compare_vars(pairs, "foo", x_vars = "first_name")

pairs <- compare_vars(pairs, "first_name", fun = jaro_winkler())

foo <- function(x, y) {
  cmp <- jaro_winkler()
  c1 <- cmp(x[[1]], y[[1]]) + cmp(x[[2]], y[[2]])
  c2 <- cmp(x[[1]], y[[2]]) + cmp(x[[2]], y[[1]])
  pmax(c1, c2)
}

pairs <- compare_vars(pairs, "foo", x_vars = c("first_name", "last_name"), fun = foo)



foo <- function(a, b) {
  data.table(
    jw = jaro_winkler()(a,b),
    ja = jaccard()(a,b)
  )
}

compare_vars(pairs, "bar", x_vars = "first_name", fun = foo, inplace = TRUE)

compare_vars(pairs, "bar", x_vars = "foo", fun = foo, inplace = TRUE)

system.time({
pairs <- compare_pairs(pairs, on = names(x)[1:8], comparators = list(
  last_name = jaro_winkler(),
  street = jaro_winkler()
))
})


fun <- function(x, y) {
  print(str(x))
  print(str(y))
  x==y
}
f <- formula(foo ~ fun(foo, bar))

all.vars(f)

f[[3]]

model.frame(f, x)



# foo <- compare_pairs(clpairs, on = names(x)[1:4], comparators = list(
#   last_name = jaro_winkler(),
#   street = jaro_winkler()
# ), new_name = "foo", overwrite = TRUE)


clusterEvalQ(cl, head(reclin_env[["default"]]$pairs))





tab <- tabulate_patterns(pairs)
tab

tabs <- tabulate_patterns(clpairs)
tabs


m <- problink_em(clpairs)
summary(m)

m
