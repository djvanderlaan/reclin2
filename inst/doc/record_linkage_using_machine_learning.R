# <unlabeled code block>
library(reclin2)

data("linkexample1", "linkexample2")
print(linkexample1)
print(linkexample2)


pairs <- pair_blocking(linkexample1, linkexample2, "postcode")

compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"), 
  inplace = TRUE, comparators = list(lastname = jaro_winkler(), 
  firstname = jaro_winkler(), address = jaro_winkler()))
print(pairs)

# <unlabeled code block>
na_as_class <- function(x, y) {
  factor(
    ifelse(is.na(x) | is.na(y), 2L, (y == x)*1L),
    levels = 0:2, labels = c("eq", "uneq", "mis"))
}

# <unlabeled code block>
pairs[, sex := NULL]

compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"), 
  inplace = TRUE, comparators = list(lastname = jaro_winkler(), 
  firstname = jaro_winkler(), address = jaro_winkler(), sex = na_as_class))
print(pairs)

# <unlabeled code block>
linkexample2$known_id <- linkexample2$id
linkexample2$known_id[c(2,5)] <- NA
setDT(linkexample2)

# <unlabeled code block>
compare_vars(pairs, "y", on_x = "id", on_y = "known_id", y = linkexample2, inplace = TRUE)

# <unlabeled code block>
compare_vars(pairs, "y_true", on_x = "id", on_y = "id", inplace = TRUE)
print(pairs)

# <unlabeled code block>
m <- glm(y ~ lastname + firstname + address + sex, data = pairs, family = binomial())

# <unlabeled code block>
pairs[, prob := predict(m, type = "response", newdata = pairs)]
pairs[, select := prob > 0.5]
table(pairs$select > 0.5, pairs$y_true)

# <unlabeled code block>
linked_data_set <- link(pairs, selection = "select", all_y = TRUE)
print(linked_data_set)

