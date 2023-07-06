<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Record linkage using machine learning}
-->
---
title: Record linkage using machine learning
author: Jan van der Laan
css: "style.css"
---


In this example we will show how `reclin2` can be used in combination with
machine learning to perform record linkage. We will use the same example as in
the introduction vignette and will skip over some of the initial steps in the
linkage project. We will use plain logistic regression. Not the most
sophisticated machine learning algorithm, but for the simplistic example more
than enough. Other algorithms are easily substituted.

When performing record linkage, we will compare combinations of records from
both datasets. After comparison we end up with a large dataset of pairs with
properties of these pairs (the comparison vectors). The goal of record linkage
is to divide these pairs into two groups: one group with pairs where both
records in the pair belong to the same object, the matching set,  and one group
where both records in the pair do not belong to the same object, the unmatched
set. Record linkage is, therefore, a classification problem and when we know for
some of the pairs if they belong to the matching set or the unmatching set, we
can use that to train a supervised classification method.


# Generate the pairs and compare

First we have to generate all pairs and compare these. This is similar as in
regular probabilistic linkage.

```{.R}
library(reclin2)

data("linkexample1", "linkexample2")
print(linkexample1)
print(linkexample2)


pairs <- pair_blocking(linkexample1, linkexample2, "postcode")

compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"), 
  inplace = TRUE, comparators = list(lastname = cmp_jarowinkler(), 
  firstname = cmp_jarowinkler(), address = cmp_jarowinkler()))
print(pairs)
```

On of the things we run into, is that the variable `sex` has missing values. We
could set these to `FALSE` (this is what is done when calling `problink_em`
during estimation of the model), but with machine learning we could also include
these as a separate category. For that we first need to define a custom
comparison function.

```{.R}
na_as_class <- function(x, y) {
  factor(
    ifelse(is.na(x) | is.na(y), 2L, (y == x)*1L),
    levels = 0:2, labels = c("eq", "uneq", "mis"))
}
```

We then remove the old variable `sex` (otherwise `compare_pairs` will complain
that we cannot assign a factor to a logical vector) and compare the pairs again
with the new comparison function.


```{.R}
pairs[, sex := NULL]

compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"), 
  inplace = TRUE, comparators = list(lastname = cmp_jarowinkler(), 
  firstname = cmp_jarowinkler(), address = cmp_jarowinkler(), sex = na_as_class))
print(pairs)
```

# Estimate the model and use the model to classify the pairs

In order to estimate the model we need some pairs for which we know the truth.
One way of obtaining this information is by reviewing some of the pairs. 
The number of pairs will generally grow with $O(N^2)$ with $N$ the size of the
smallest dataset. The number of matches in these pairs is usually $O(N)$.
Therefore, the fraction of matches in the pairs is $O(1/N)$ and therefore
usually very small. Therefore, when sampling records for review it is usually a
good idea to not sample the pairs completely random, but, for example,
oversample pairs that agree on more variables. 

Another way of getting a training dataset is when additional information is
available. For example, when linking a dataset to a population register for some
of the records in the dataset an official id might be available. For these
records the true match status can be determined. This is what we will simulate
in the example below. Let's assume we know from three of the records in
`linkexample2` the `id`:

```{.R}
linkexample2$known_id <- linkexample2$id
linkexample2$known_id[c(2,5)] <- NA
setDT(linkexample2)
```

We the know for these records the true match status in the pairs. Below we add
this to the pairs:
```{.R}
compare_vars(pairs, "y", on_x = "id", on_y = "known_id", y = linkexample2, inplace = TRUE)
```
Note that we supply `y = linkexample2` in the call. This is needed as the copy
of `linkexample2` stored with `pairs` does not contain the `known_id` column. We
can also add the true status for all records to measure the performance of the
linkage in the end
```{.R}
compare_vars(pairs, "y_true", on_x = "id", on_y = "id", inplace = TRUE)
print(pairs)
```

We now have all of the information needed to estimate our (machine learning)
model. Note that this will give a bunch of warnings as we estimating six
parameters with only eleven observations and the parameters will not be reliably
estimated.

```{.R}
m <- glm(y ~ lastname + firstname + address + sex, data = pairs, family = binomial())
```

And then we can add the prediction to `pairs` and check how well we have done:

```{.R}
pairs[, prob := predict(m, type = "response", newdata = pairs)]
pairs[, select := prob > 0.5]
table(pairs$select > 0.5, pairs$y_true)
```

Given the small size of the dataset we have to estimate the model on, this is
not too bad. 


# Create the linked data set

We now know which pairs are to be linked, but we still have to actually link
them. `link` does that (the optional arguments `all_x` and `all_y` control the
type of linkage):

```{.R}
linked_data_set <- link(pairs, selection = "select", all_y = TRUE)
print(linked_data_set)
```

