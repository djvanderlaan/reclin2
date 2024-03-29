% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_tabulate_patterns.R,
%   R/tabulate_patterns.R
\name{tabulate_patterns.cluster_pairs}
\alias{tabulate_patterns.cluster_pairs}
\alias{tabulate_patterns}
\alias{tabulate_patterns.pairs}
\title{Create a table of comparison patterns}
\usage{
\method{tabulate_patterns}{cluster_pairs}(pairs, on, comparators, complete = TRUE, ...)

tabulate_patterns(pairs, on, comparators, complete = TRUE, ...)

\method{tabulate_patterns}{pairs}(pairs, on, comparators, complete = TRUE, ...)
}
\arguments{
\item{pairs}{a \code{pairs} object, such as generated by 
\code{\link{pair_blocking}}}

\item{on}{variables from \code{pairs} defining the comparison patterns. When
missing \code{names(comparators)} is used.}

\item{comparators}{a list with comparison functions for each of the 
columns. When missing or \code{NULL}, the function looks for columns in 
\code{pairs} with a \code{comparator} attribute.}

\item{complete}{add patterns that do not occur in the dataset to the result 
(with \code{n = 0}).}

\item{...}{passed on to other methods.}
}
\value{
Returns a \code{data.frame} with all unique comparison patterns that exist
in \code{pairs}, with a column \code{n} added with the number of times each
pattern occurs.
}
\description{
Create a table of comparison patterns
}
\details{
Since comparison vectors can contain continuous numbers (usually between 0
and 1), this could result in a very large number of possible comparison 
vectors. Therefore, the comparison vectors are passed on to the comparators
in order to threshold them. This usually results in values 0 or 1. Missing
values are usually codes as 0. However, this all depends on the comparison
functions used. For more information see the documentation on the 
\link[=cmp_identical]{comparison functions}.
}
\examples{
data("linkexample1", "linkexample2")
pairs <- pair_blocking(linkexample1, linkexample2, "postcode")
pairs <- compare_pairs(pairs, c("lastname", "firstname", "address", "sex"))
tabulate_patterns(pairs)

}
