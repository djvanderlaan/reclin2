# <unlabeled code block>
library(reclin2)

# <unlabeled code block>
data("linkexample1", "linkexample2")
print(linkexample1)
print(linkexample2)

# <unlabeled code block>
pairs <- pair_blocking(linkexample1, linkexample2, "postcode")
print(pairs)

# <unlabeled code block>
pairs <- compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"))
print(pairs)

# <unlabeled code block>
compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"), 
  inplace = TRUE)
print(pairs)

# <unlabeled code block>
compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"),
  default_comparator = jaro_winkler(0.9), inplace = TRUE)
print(pairs)

# <unlabeled code block>
m <- problink_em(~ lastname + firstname + address + sex, data = pairs)
print(m)

# <unlabeled code block>
pairs <- predict(m, pairs = pairs, add = TRUE)
print(pairs)

# <unlabeled code block>
pairs <- select_threshold(pairs, "threshold", score = "weights", threshold = 8)
print(pairs)

# <unlabeled code block>
pairs <- compare_vars(pairs, "truth", on_x = "id", on_y = "id")
print(pairs)

# <unlabeled code block>
table(pairs$truth, pairs$threshold)

# <unlabeled code block>
pairs <- select_greedy(pairs, "weights", variable = "greedy", threshold = 0)
table(pairs$truth, pairs$greedy)

# <unlabeled code block>
pairs <- select_n_to_m(pairs, "weights", variable = "ntom", threshold = 0)
table(pairs$truth, pairs$ntom)

# <unlabeled code block>
linked_data_set <- link(pairs, selection = "ntom")
print(linked_data_set)

