
library(reclin2)

x <- fread("work/x.csv", stringsAsFactors = FALSE, data.table = TRUE)
y <- fread("work/y.csv", stringsAsFactors = FALSE, data.table = TRUE)


tpair <- system.time({
  pairs <- pair_blocking(x, y, on = "postcode")
})

message("Number of pairs: ", nrow(pairs))

tcompare <- system.time({
  vars <- c("last_name", "street", "number", "first_name", "dob_yr", 
    "dob_mo", "dob_dy")
  compare_pairs(pairs, vars, inplace = TRUE, 
    comparators = list(last_name = jaro_winkler(0.85) ))
})

tem <- system.time({
  tab <- tabulate_patterns(pairs, on = vars)
  f <- formula(~ last_name + street + number + first_name + dob_dy)
  m <- problink_em(f, patterns = tab)
  pairs <- predict(m, pairs = pairs, type = "all", add = TRUE)
})

tselect <- system.time({
  select_greedy(pairs, "selected_greedy", "weight", threshold = 5, 
    inplace = TRUE)
})

tlink <- system.time({
  res <- link(pairs, selection = "selected_greedy", x = x, y = y, 
    all_x = TRUE, all_y = FALSE, keep_from_pairs = "weight")
})


message("Timings:")
message("Pair:")
print(tpair)
message("Compare:")
print(tcompare)
message("EM:")
print(tem)
message("Select:")
print(tselect)
message("Link:")
print(tlink)

