<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Deduplication using reclin2}
-->
---
title: Deduplication using reclin2
author: Jan van der Laan
css: "style.css"
---


We are going to work with the dataset `town_name` included in the package. The
dataset contains a collection of town names as observed in administrative
dataset. The first column `name` contains the names as observed. The second
column `official_name` the official town name. We are going to assume that the
second column is not available (or only for a part of the observations). The
goal is to recode the 584 town names into a smaller set of town names knowing
that most of the observed town names are actually misspelled versions of a
smaller set of town names. We could also have solved the problem differently by
linking the observed town names to a dataset containing all official town names.
Often cleaning up these kind of misspellings is a first step in an actual
linkage process. By first cleaning up the town names, subsequent use of the
variable is easier and can lead to better quality linkage.

```{.R}
library(reclin2)
data(town_names)
head(town_names)
```

When performing deduplication we will link a dataset to itself and will try to
link different records belonging to the same object. When a dataset to itself, it
is not necessary to both compare record *i* to *j* and *j* to *i* and we
certainly do not want to compare a record to itself. The option `deduplication`
of the `pair_` functions makes sure that only the needed pairs are generated.
This is a small dataset so we can easily generate all pairs:

```{.R}
pairs <- pair(town_names, deduplication = TRUE)
print(pairs)
```

We will compare the records on `name` and use a string similarity function. 

```{.R}
compare_pairs(pairs, on = "name", 
  comparators = list(jaro_winkler()), 
  inplace = TRUE)
print(pairs)
```

Now comes the difficult part: selecting a threshold. The problem is that it is
not really possible to say beforehand what an appropriate threshold is. That
depends on the exact problem and also depends on the number of different
objects that are expected. To explain that, first a short explanation how the
`deduplicate_equivalence` function that we are going to use later works. Let's
assume we have two actual town names and using our string similarity function
we select pairs that differ one letter from each other, so we end up with the
following set of pairs as an example

```
rotterdam -> rottrdam
rotterdam -> rotterdm
rotterdm -> rottrdm
rtterdam -> rotterdam
amsterdam -> amstrdam
amstrdam -> amstdam
amsterdm -> amsterdam
```

That means that we are saying that `rotterdam` is the same object as `rottrdam`
which is the same object as `rottrdm`. Therefore, `rotterdam` and `rottrdm` are
the same object although we didn't select a pair `rotterdam -> rottrdm`. So all
names `rotterdam`, `rottrdam`, `rotterdm`, `rottrdm` and `rtterdam` are going to
be in one class. When the number of misspelled names increases and when the
number of actual town names increases, the likelihood that two names that do not
belong to the same object are linked by a chain of pairs increases.  This is a
bit like the game where you have to change one word into another in a given number
of steps by changing one letter at a time (the words in between have to be valid
words). When the vocabulary is bigger this becomes easier. Therefore, the
optimal threshold depends on the number of actual town names and the number of
misspellings. 

We have the official names and can therefore measure how many errors we make. We
make an error when we put two records from `x` in the same group while they
actually belong to different object (official town names). First we add a
variable indicating whether two pairs have the same official name:

```{.R}
compare_vars(pairs, "true", on_x = "official_name",
  inplace = TRUE)
```

In practice this information is not available, but it might be available for a
subset of records, for example, after manual inspection of a subset of the
pairs. We now round the similarity scores and count how many errors we make for
each value of the similarity score threshold:

```{.R}
pairs$threshold <- trunc(pairs$name/0.05)*0.05
thresholds <- pairs[, .(ftrue = mean(true)), by = threshold]
print(thresholds[order(ftrue)])
```

For a threshold of 0.95 and 1.00 we make no errors. Below that we start making
errors. So let's work with a threshold of 0.95 for now

```{.R}
select_threshold(pairs, "select", "name", threshold = 0.95,
  inplace = TRUE)
res <- deduplicate_equivalence(pairs, "group", "select")
print(res)
```

With `deduplicate_equivalence` we take all selected pairs (indicated by the
column `select`) and put them in the same group.  
`res` now contains the original dataset with a `group` column added that
indicates the unique objects (towns in this case). We can see how many towns we
have in the resulting dataset:

```{.R}
length(unique(res$group))
```

This is quite large. We started with `nrow(res)`{.R} town names and reduced that
to `length(unique(res$group))`{.R} while there are actually
`length(unique(res$official_name))`{.R} town names. We can measure the quality
by counting how often we have more than one official town name in one group:

```{.R}
qual <- res[, .(errors = length(unique(official_name))-1, n = .N), by = group]
qual$ferrors <- qual$errors/qual$n
qual[errors > 0]
```
So we have a large number of groups and no errors: no town names have been 
classified in the same group while actually being different towns. We can check
what happens when we decrease the threshold. We will probably introduce some
errors while we decrease the number of groups:

```{.R}
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
```

The results are plotted in the figure below.

```{.R fun=output_figure name="fig2"}
opar = par(mfrow = c(2,2))
plot(thresholds, sizes)
plot(thresholds, nerrors)
plot(sizes, nerrors)
par(opar)
```

We can see that as the threshold decreases the number of errors increases and
the number of groups decreases. We cannot get much less than the 161 groups we
found without introducing some errors. How many errors and/or groups are
acceptable depends on the application and the amount of time one s willing to
spend in manually merging the groups. In this case manually inspecting the
groups and merging them will probably take only a few hours and 

With a threshold of 0.9 we should get approximately 100 groups and 5 errors
which seems a reasonable trade-off. So, let's rerun some of the previous code
with a threshold of 0.90. 

```{.R}
select_threshold(pairs, "select", "name", threshold = 0.9,
  inplace = TRUE)
res <- deduplicate_equivalence(pairs, "group", "select")
qual <- res[, .(errors = length(unique(official_name))-1, n = .N), by = group]
qual$ferrors <- qual$errors/qual$n
qual[errors > 0]
```


One way of assigning names to the groups we derived, is to use the most frequent
name used in the group. Assuming that most people will correctly spell the town
names this should give us the official town name belonging to each group. In
this example dataset each town name occurs only once so can't use that trick.
However, we can use the most frequent official name. We first define a function
that returns the most frequent value of a vector and use that to derive the
name of the group. 

```{.R}
most_frequent <- function(x) {
  t <- table(x)
  t <- sort(t)
  tail(names(t), 1)
}

res[, assigned_name := most_frequent(official_name), by = group]
print(res)
```

We can now also look at the errors:

```{.R}
print(res[assigned_name != official_name])
```

We see that we make a lot of errors with the town of `Hoogvliet Rotterdam`. The
problem we have is a difficult one. For example, `rotterdam charlois` should be
called `Rotterdam` while `rotterdam hoogvliet` should be called `Hoogvliet
Rotterdam`. We can't really expect that a computer is able to distinguish
between these two without additional information. One other way of solving this
problem is actually consider this as a linkage problem: we want to link a set of
written town names to an official set of town names.

