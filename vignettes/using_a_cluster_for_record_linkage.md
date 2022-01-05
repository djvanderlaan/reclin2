<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Using a cluster for record linkage}
-->

---
title: Using a cluster for record linkage
author: Jan van der Laan
css: "style.css"
---

## Introduction

`reclin2` has the functionality to use a cluster created by `parallel` or `snow`
for record linkage. There are a couple of advantages to this. First, record
linkage can be a computationally intensive problem as all records from both
datasets have to be compared to each other. Splitting the computation over
multiple cores or CPU's can give a substantial speed benefit. The problem easily
to parallelize. Second, when using a `snow` cluster, the computation can be
distributed over multiple machines allowing `reclin2` to use the memory of these
multiple machined. Besides computationally intensive, record linkage can also be
memory intensive as all pairs are stored in memory. 

Parallelization over `k` cluster nodes is realised by randomly splitting the
first dataset `x` into `k` equally sized parts and distribution over the nodes.
The second dataset `y` is copied to each of the nodes. Therefore, it is
beneficial for memory consumption if the first dataset is the largest of the
two. On each node the local `y` is compared to the local `x` and a local set of
pairs is generated. For most operations there exist methods for `cluster_pairs`.
These usually consist of running the operations for the regular `pairs` on each
of the nodes. 

Below an example is given using a small cluster. It is assumed that the reader
has read the introduction vignette and knows the general procedure of record
linkage. 


## Basic example

In this example the example in the introduction vignette is repeated using a
cluster. 


```{.R}
library(reclin2)
```

We will work with a pair of data sets with artificial data. They are tiny, but
that allows us to see what happens. In this example we will perform 'classic'
probabilistic record linkage. 

```{.R}
data("linkexample1", "linkexample2")
print(linkexample1)
print(linkexample2)
```

We first have to start a cluster. Pairs can then be generated using any of the
`cluster_pair_*` functions. 

```{.R}
library(parallel)
cl <- makeCluster(2)
pairs <- cluster_pair_blocking(cl, linkexample1, linkexample2, "postcode")
print(pairs)
```

The print function collects a few (max 6) pairs from each of the nodes and shows
those. Other `cluster_pair_*` functions are `cluster_pair` and
`cluster_pair_minsim`. 

The `cluster_pair_*` functions return an object of type `cluster_pairs`. Most
other methods work the same as for regular pairs. For example, to compare the
pairs on variables:

```{.R}
compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"), 
  default_comparator = jaro_winkler(0.9), inplace = TRUE)
print(pairs)
```

The code above was copy-pasted from the introduction. Here the argument 
`inplace = TRUE` was used, which adds the new variables to the existing pairs.
One difference between regular `pairs` and `cluster_pairs` is that most methods
will modify the existing pairs in place. Therefore, `inplace` is ignored here
and we should use:

```{.R}
compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"),
  default_comparator = jaro_winkler(0.9))
print(pairs)
```

Most methods for `cluster_pairs` do have a `new_name` argument that will
generate a new set of pairs on the cluster nodes. For example, the following
code will generate a new set of pairs and will not modify the existing pairs:

```{.R}
pairs2 <- compare_pairs(pairs, on = 
  c("lastname", "firstname", "address", "sex"), new_name = "pairs2")
print(pairs2)
print(pairs)
```

The function `compare_vars` offers more flexibility than `compare_pairs`. It can
for example compare multiple variables at the same time (e.g. compare birth day
and month allowing for swaps) or generate multiple results from comparing on one
variable. This method also works on `cluster_pairs`. 



The next step in the process, is to determine which pairs of records belong to
the same entity and which do not. As in the introduction vignette we will use
the classic method. Again, we hardly need to change the code from the
introduction:

```{.R}
m <- problink_em(~ lastname + firstname + address + sex, data = pairs)
print(m)
pairs <- predict(m, pairs = pairs, add = TRUE)
print(pairs)
```

We can then select the pairs with a weight above a threshold.

```{.R}
pairs <- select_threshold(pairs, "threshold", score = "weights", threshold = 8)
print(pairs)
```

And this is roughly where we have to stop working with `cluster_pairs`. The
subset of selected pairs remaining should now be small enough that we can
comfortably work locally. The most computationally intensive steps have been
done. When we are not sure exactly what the threshold should be, we can also
work with a more conservative threshold. That should still give us enough of a
reduction in pairs that we can work locally. Using `cluster_collect` we can copy
the selected pairs (or all pairs) locally:

```{.R}
pairs <- select_threshold(pairs, "threshold", score = "weights", threshold = 0)
local_pairs <- cluster_collect(pairs, "threshold")
print(local_pairs)
```

`local_pairs` is a regular `pairs` object (and therefore a `data.table`) which
can be operated upon as any `pairs` object. `cluster_collect` also has the
option `clear` which when `TRUE` will delete the pairs on the cluster nodes.
After this we can use the code from the introduction vignette:

```{.R}
local_pairs <- compare_vars(local_pairs, "truth", on_x = "id", on_y = "id")
local_pairs <- select_n_to_m(local_pairs, "weights", variable = "ntom", threshold = 0)
table(local_pairs$truth, local_pairs$ntom)
linked_data_set <- link(local_pairs, selection = "ntom")
print(linked_data_set)
```

## Internals

The `cluster_pair` object is a list with two elements: 

- `cluster` with a copy of the `parallel` or `snow` cluster. 
- `name` the name of the environment on the cluster nodes in which the pairs are
  stored.

On the cluster nodes there exists an environment (`reclin2::reclin_env`). For
each set of pairs an environment is created in that environment containing the
pairs. To demonstrate, let us get the first pair on each of the nodes:

```{.R}
clusterCall(pairs$cluster, function(name) {
  pairs <- reclin2:::reclin_env[[name]]$pairs
  head(pairs, 1)
}, name = pairs$name)
```

## Some specific methods for `cluster_pairs`

Regular `pairs` are also a `data.table`. Therefore, it is easy to manually
create columns, select or aggregate. As for `cluster_pairs` the pairs are
distributed over the cluster nodes, this is more difficult for `cluster_pairs`.
In order to help with this, `reclin2` has two helper functions: `cluster_call`
and `cluster_modify_pairs`. 

You can pass `cluster_call` the `cluster_pairs` object and a function. This
function will be called on each cluster node and will be passed the `pairs`
object, the local `x` and `y` (in that order). This can be used to modify the
pairs, or calculate statistics from the pairs. The result of the function calls
is returned by `cluster_call`. Therefore, if the sole goal is to modify the
pairs, make sure to return `NULL` (or at least something small). Below we use
`cluster_call` to make a random stratified sample of pairs:

```{.R}
compare_vars(pairs, "id")

cluster_call(pairs, function(pairs, ...) {
  sel1 <- sample(which(pairs$id), 2)
  sel2 <- sample(which(!pairs$id), 2)
  pairs[, sample := FALSE]
  pairs[c(sel1, sel2), sample := TRUE]
  NULL
})

sample <- cluster_collect(pairs, "sample")
```

`cluster_modify_pairs` is very similar to `cluster_call` but is mainly meant for
modifying the pairs object. Although in the previous example we also used
`cluster_call` for that. When the function passed to `cluster_modify_pairs`
returns a `data.table`, this `data.table` will overwrite the `pairs` object.
`cluster_modify_pairs` also accepts a `new_name` argument. When set a new pairs
object will be created.

Let's use the sample from above to estimate a model and then use
`cluster_modify_pairs` to add the predictions to the pairs:

```{.R}
mglm  <- glm(id ~ lastname + firstname, data = sample)

cluster_modify_pairs(pairs, function(pairs, model, ...) {
  pairs$pmodel <- predict(model, newdata = pairs, type = "response")
  pairs
}, model = mglm)
```


