
library(reclin)
library(dplyr)

x <- data.table::fread("work/x.csv", stringsAsFactors = FALSE)
y <- data.table::fread("work/y.csv", stringsAsFactors = FALSE)



tpair <- system.time({
  pairs <- pair_blocking(x, y, "postcode", large = FALSE)
})

message("Number of pairs: ", nrow(pairs))

tcompare <- system.time({
  vars <- c("last_name", "street", "number", "first_name", "dob_yr", 
    "dob_mo", "dob_dy")
  pairs <- compare_pairs(pairs, by = vars, 
    comparators = list(last_name = jaro_winkler(0.85) ))
})

tem <- system.time({
  m <- problink_em(pairs)
  pairs <- score_problink(pairs, model = m, add = TRUE, var = "weight")
})

tselect <- system.time({
  pairs <- select_greedy(pairs, "weight", var = "greedy", threshold = 5)
})

tlink <- system.time({
  res <- link(pairs)
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

