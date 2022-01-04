<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Introduction to reclin2}
-->

---
title: Introduction to reclin2
author: Jan van der Laan
css: "style.css"
---

## Introduction

`reclin2` implements methodology for linking records based on inexact keys. It
allows for maximum flexibility by giving users full control over each step of
the linking procedure.  The package is built with performance and scalability
in mind: the core algorithms have been implemented in `C++`. 

```{.R}
library(reclin2)
```

We will work with a pair of data sets with artificial data. They are tiny, but
that allows us to see what happens. In this example we will perform 'classic'
probabilistic record linkage. When some known true links are known it is also
possible to use machine learning methods. This is illustrated in another
vignette. 

```{.R}
data("linkexample1", "linkexample2")
print(linkexample1)
print(linkexample2)
```

We have two data sets with personal information. The second data set contains a
lot of errors, but we will try to link the second data set to the first.

## Step 1: generate pairs

In principle linkage consists of comparing each combination of records from the
two data sets and determine which of those combinations (or pairs as we will
call them below) belong to the same entity. In case of a perfect linkage key, it
is of course, not necessary to compare all combinations of records, but when
the linkage keys are imperfect and contain errors, it is in principle necessary
to compare all pairs.

However, comparing all pairs can result in an intractable number of
pairs: when linking two data sets with a million records there are $10^{12}$
possible pairs. Therefore, some sort of reduction of the possible pairs is
usually applied. In the example below, we apply *blocking*, which means that
pairs are only generated when they agree on the blocking variable (in this case
the postcode). This means that pairs of records that disagree on the blocking
variable are not considered. Therefore, one will only use variables that can
be considered without errors as blocking variable, or link multiple times with
different blocking variables and combine both data sets.

The first step in (probabilistic) linkage is, therefore, generating all pairs:
```{.R}
pairs <- pair_blocking(linkexample1, linkexample2, "postcode")
print(pairs)
```

As you can see, record 1 from `x` (the first data set) is compared to records
1, 2 and 3 from `y`. Also note that `reclin2` uses the `data.table` package to
efficiently perform some of the computations. Therefore, the `pairs` object is a
`data.table`. 

Other functions to generate pairs are:

- `pair`: generate all possible pairs
- `pairs_minsim`: generate pairs that have minimum similarity score (e.g. should
  agree on at least one variable in a set of given variables). Can be
  computationally intensive as all records have to be compared. 

## Step 2: compare pairs

We can now compare the records on their linkage keys:

```{.R}
pairs <- compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"))
print(pairs)
```

As you can see, we don't need to pass the original data sets although the
variables `lastname` etc. are from those original data sets. This is because a
copy of the original data sets are stored with the pairs object `pairs` (and should
you be worrying about memory: as long as the original data sets are not
modified the data sets are not actually copied).

In the example above the result of `compare_pairs` was assigned back to `pairs`.
When working with large datasets it can be more efficient to modify `pairs` 
in place preventing unnecessary copies. This behaviour can be switched on using
the `inplace` argument which is accepted by most functions.

```{.R}
compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"), 
  inplace = TRUE)
print(pairs)
```

The default comparison function returns `TRUE` when the linkage keys agree and
false when they don't. However, when looking at the original data sets, we can
see that most of our linkage keys are string variables that contain typing
errors. The quality of our linkage could be improved if we could use a
similarity score to compare the two strings: a high score means that the two
strings are very similar a value close to zero means that the strings are very
different.

Below we use the `jaro_winkler` similarity score to compare all fields:

```{.R}
compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"),
  default_comparator = jaro_winkler(0.9), inplace = TRUE)
print(pairs)
```

The function `compare_vars` offers more flexibility than `compare_pairs`. It can
for example compare multiple variables at the same time (e.g. compare birth day
and month allowing for swaps) or generate multiple results from comparing on one
variable.

## Step 3: score pairs

The next step in the process, is to determine which pairs of records belong to
the same entity and which do not. There are numerous ways to do this. One
possibility is to label some of the pairs as match or no match, and use some
machine learning algorithm to predict the match status using the comparison
vectors. Another, method, is to score the pairs based on the comparison vectors
and select those with a score above some threshold. The simplest way to score
the pairs, is to calculate the sum of the comparison vectors. That is what
`score_simsum` does:

```{.R}
#pairs <- score_simsum(p, var = "simsum")
#print(pairs)
```

The disadvantage of `score_simsum` is that it doesn't take into account that
the amount of information in agreement or disagreement on a variable depends
on the variable. For example, agreement on sex doesn't tell us much: when
our data sets contain 50% men an 50% women, there is a 50% chance that two
random records agree on sex. On the other hand the probability that two random
records agree on last name is much lower. Therefore, agreement on last name makes
it much more likely that the two records belong to the same entity.

This is what the probabilistic linkage framework initially formalised by Fellegi
and Sunter tries to do. The function `problink_em` uses an EM-algorithm to
estimate the so called m- and u-probabilities for each of the linkage variables.
The m-probability is the probability that two records concerning the same entity
agree on the linkage variable; this means that the m-probability corresponds to
the probability that there is an error in the linkage variables.
The u-probability is the probability that two records belonging to different
entities agree on a variable. For a variable with few categories (such as sex)
this probability will be large, while for a variable with a large number of
categories (such as last name) this probability will be small.

```{.R}
m <- problink_em(~ lastname + firstname + address + sex, data = pairs)
print(m)
```

These m- and u-probabilities can be used to score the pairs:

```{.R}
pairs <- predict(m, pairs = pairs, add = TRUE)
print(pairs)
```

With `add = TRUE` the predictions are added to the `pairs` object.  The higher
the weight the more likely the two pairs belong to the same entity/are a match.

The prediction function can also return the m- and u-probabilities and the 
posterior m- and u-probabilities.

## Step 4: select pairs

The final step is to select the pairs that are considered to belong to the
same entities. The simplest method is to select all pairs above a certain
threshold

```{.R}
pairs <- select_threshold(pairs, "threshold", score = "weights", threshold = 8)
print(pairs)
```

The select functions add a (logical) variable to the data set indicating
whether a pairs is selected or not.

In this case we know which records truly belong to each other. We can use that
to evaluate the linkage:
```{.R}
pairs <- compare_vars(pairs, "truth", on_x = "id", on_y = "id")
print(pairs)
```
```{.R}
table(pairs$truth, pairs$threshold)
```

We see that three of the four matches that should have been found have indeed
been found (the recall is 3/4) and we have one false link (sensitivity is 1/4).

Using a threshold, does not take into account the fact that often we know that
one record from the first data set can be linked to at most one record from the
second data set and vice versa. If we make the threshold low enough we have more
links than records in either data set. `reclin` contains two functions that
force one-to-one linkage: `select_greedy` and `select_n_to_m`. The first is
fast (it selects pairs starting from the highest score; pairs are only selected
when each of the records in a pair have not been selected previously); the
second is slower, but can lead to better results (it tries to optimise to total
score of the selected records under the restriction that each record can be
selected only once):


```{.R}
pairs <- select_greedy(pairs, "weights", variable = "greedy", threshold = 0)
table(pairs$truth, pairs$greedy)
```


```{.R}
pairs <- select_n_to_m(pairs, "weights", variable = "ntom", threshold = 0)
table(pairs$truth, pairs$ntom)
```

Perfection!

## The final, last step

The real final step is to create the linked data set. We now know which pairs
are to be linked, but we still have to actually link them. `link` does that (the
optional arguments `all_x` and `all_y` control the type of linkage):

```{.R}
linked_data_set <- link(pairs, selection = "ntom")
print(linked_data_set)
```


