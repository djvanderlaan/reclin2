library(devtools)
load_all()



source("work/random_data.R")

n <- 5000
dta <- random_data(n1 = n, n2 = n*0.8, overlap = 0.2)

x <- as.data.table(dta[[1]])
y <- as.data.table(dta[[2]])


#files <- list.files("R", "*.R", full.names = TRUE)
#for (file in files) source(file)


cl <- makeCluster(4)

# pairs <- pair(x, y)

# pairs <- pair_blocking(x, y, on = c("postcode"))

system.time({
  pairs <- pair_minsim(x, y, on = names(x)[1:8], minsim = 2)
  nrow(pairs)
})

system.time({
  pairs <- pair_blocking(x, y, on = "postcode")
})


system.time({
  clpairs <- cluster_pair_minsim(cluster = cl, x, y, on = names(x)[1:8], minsim = 2)
})

system.time({
  clpairs <- cluster_pair_blocking(cluster = cl, x, y, on = "postcode")
})


system.time({
pairs <- compare_pairs(pairs, on = names(x)[1:8], comparators = list(
  last_name = jaro_winkler(),
  street = jaro_winkler()
))
})

system.time({
clpairs <- compare_pairs(clpairs, on = names(x)[1:8], comparators = list(
  last_name = jaro_winkler(),
  street = jaro_winkler()
), overwrite = TRUE)
})



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


