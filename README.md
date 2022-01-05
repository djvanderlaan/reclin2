# `reclin2`

`reclin2` is a package for record linkage an deduplication. The package is an
update to the `reclin` package. As the package is not backwards compatible with
`reclin` and `reclin` still has some features that are not present in `reclin2`
it was decided to release the package under a new name.

The focus of `reclin2` is on performance, memory and CPU and flexibility. To get
the performance `reclin2` uses `data.table` for most of its computations and
`reclin2` has the ability to spread its computations over multiple CPU cores or
machines. In principle record linkage can easily be sped up using
parallelization and by using multiple machines using the `snow` package data can
be distributed over multiple machines thereby making use of the memory available
on those machines.

Each record linkage project often has its own idiosyncrasies. Therefore, it is
important that users are able to customise parts of the linkage process.
`reclin2` is designed as a kind of toolkit for record linkage. It has functions
and methods for different parts of the linkage process. Users are able to mix
these different functions to get a custom record linkage process. Furthermore,
`reclin2` uses relatively simple data structures. The core data structure is a
`data.table` with pairs and the properties of these pairs. Therefore, users can
relatively easy manipulate this data and write custom functions that manipulate
this data. 

Many of the features can be found in the vignettes of the package:

- [Introduction to reclin2](https://htmlpreview.github.io/?https://github.com/djvanderlaan/reclin2/blob/master/inst/doc/introduction.html)
- [Record linkage using machine learning](https://htmlpreview.github.io/?https://github.com/djvanderlaan/reclin2/blob/master/inst/doc/record_linkage_using_machine_learning.html)
- [Deduplication using reclin2](https://htmlpreview.github.io/?https://github.com/djvanderlaan/reclin2/blob/master/inst/doc/deduplication.html)
- [Using a cluster for record linkage](https://htmlpreview.github.io/?https://github.com/djvanderlaan/reclin2/blob/master/inst/doc/using_a_cluster_for_record_linkage.html)



