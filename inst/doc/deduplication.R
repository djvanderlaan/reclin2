# <unlabeled code block>
library(reclin2)
data(town_names)
head(town_names)

# <unlabeled code block>
pairs <- pair(town_names, deduplication = TRUE)
print(pairs)

# <unlabeled code block>
compare_pairs(pairs, on = "name", 
  comparators = list(cmp_jarowinkler()), 
  inplace = TRUE)
print(pairs)

# <unlabeled code block>
compare_vars(pairs, "true", on_x = "official_name",
  inplace = TRUE)

# <unlabeled code block>
pairs$threshold <- trunc(pairs$name/0.05)*0.05
thresholds <- pairs[, .(ftrue = mean(true)), by = threshold]
print(thresholds[order(ftrue)])

# <unlabeled code block>
select_threshold(pairs, "select", "name", threshold = 0.95,
  inplace = TRUE)
res <- deduplicate_equivalence(pairs, "group", "select")
print(res)

# <unlabeled code block>
length(unique(res$group))

# <unlabeled code block>
qual <- res[, .(errors = length(unique(official_name))-1, n = .N), by = group]
qual$ferrors <- qual$errors/qual$n
qual[errors > 0]

# <unlabeled code block>
# Create a sequence of thresholds and initialise the result vectors
thresholds <- seq(0.5, 1, by = 0.02)
sizes <- numeric(length(thresholds))
nerrors <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  # Perform deduplication with the given threshold
  select_threshold(pairs, "select", "name", threshold = threshold, inplace = TRUE)
  res <- deduplicate_equivalence(pairs, "group", "select")
  # Count the number of unique groups
  sizes[i] <- length(unique(res$group))
  # Count the number of errors
  qual <- res[, .(errors = length(unique(official_name))-1, n = .N), by = group]
  nerrors[i] <- sum(qual$errors)
}

# <unlabeled code block>
opar = par(mfrow = c(2,2))
plot(thresholds, sizes)
plot(thresholds, nerrors)
plot(sizes, nerrors)
par(opar)

# <unlabeled code block>
select_threshold(pairs, "select", "name", threshold = 0.9,
  inplace = TRUE)
res <- deduplicate_equivalence(pairs, "group", "select")
qual <- res[, .(errors = length(unique(official_name))-1, n = .N), by = group]
qual$ferrors <- qual$errors/qual$n
qual[errors > 0]

# <unlabeled code block>
most_frequent <- function(x) {
  t <- table(x)
  t <- sort(t)
  tail(names(t), 1)
}

res[, assigned_name := most_frequent(official_name), by = group]
print(res)

# <unlabeled code block>
print(res[assigned_name != official_name])

