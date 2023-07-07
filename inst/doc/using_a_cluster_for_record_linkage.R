# <unlabeled code block>
library(reclin2)

# <unlabeled code block>
data("linkexample1", "linkexample2")
print(linkexample1)
print(linkexample2)

# <unlabeled code block>
library(parallel)
cl <- makeCluster(2)
pairs <- cluster_pair_blocking(cl, linkexample1, linkexample2, "postcode")
print(pairs)

# <unlabeled code block>
compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"), 
  default_comparator = cmp_jarowinkler(0.9), inplace = TRUE)
print(pairs)

# <unlabeled code block>
compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"),
  default_comparator = cmp_jarowinkler(0.9))
print(pairs)

# <unlabeled code block>
pairs2 <- compare_pairs(pairs, on = 
  c("lastname", "firstname", "address", "sex"), new_name = "pairs2")
print(pairs2)
print(pairs)

# <unlabeled code block>
m <- problink_em(~ lastname + firstname + address + sex, data = pairs)
print(m)
pairs <- predict(m, pairs = pairs, add = TRUE)
print(pairs)

# <unlabeled code block>
pairs <- select_threshold(pairs, "threshold", score = "weights", threshold = 8)
print(pairs)

# <unlabeled code block>
pairs <- select_threshold(pairs, "threshold", score = "weights", threshold = 0)
local_pairs <- cluster_collect(pairs, "threshold")
print(local_pairs)

# <unlabeled code block>
local_pairs <- compare_vars(local_pairs, "truth", on_x = "id", on_y = "id")
local_pairs <- select_n_to_m(local_pairs, "weights", variable = "ntom", threshold = 0)
table(local_pairs$truth, local_pairs$ntom)
linked_data_set <- link(local_pairs, selection = "ntom")
print(linked_data_set)

# <unlabeled code block>
clusterCall(pairs$cluster, function(name) {
  pairs <- reclin2:::reclin_env[[name]]$pairs
  head(pairs, 1)
}, name = pairs$name)

# <unlabeled code block>
compare_vars(pairs, "id")

cluster_call(pairs, function(pairs, ...) {
  sel1 <- sample(which(pairs$id), 2)
  sel2 <- sample(which(!pairs$id), 2)
  pairs[, sample := FALSE]
  pairs[c(sel1, sel2), sample := TRUE]
  NULL
})

sample <- cluster_collect(pairs, "sample")

# <unlabeled code block>
mglm  <- glm(id ~ lastname + firstname, data = sample)

cluster_modify_pairs(pairs, function(pairs, model, ...) {
  pairs$pmodel <- predict(model, newdata = pairs, type = "response")
  pairs
}, model = mglm)

# <unlabeled code block>
stopCluster(cl)

