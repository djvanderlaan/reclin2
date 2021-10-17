

library(data.table)

source("work/random_data.R")
set.seed(1)
n <- 5000000
dta <- random_data(n1 = n, n2 = n*0.8, overlap = 0.2, perr = 0.2)
x <- as.data.table(dta[[1]])
y <- as.data.table(dta[[2]])

fwrite(x, "work/x.csv")
fwrite(y, "work/y.csv")

