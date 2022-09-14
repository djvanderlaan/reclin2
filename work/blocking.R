library(reclin2)

data(linkexample1)
data(linkexample2)

linkexample1
linkexample2

linkexample1$postcode[1] <- NA
linkexample1$postcode[3] <- "6789 XY"


res <- pair_minsim(linkexample1, linkexample2, on = c("postcode", "lastname"), minsim = 0.5)

data.table(
  .x = c(1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L), 
  .y = c(1L, 1L, 2L, 3L, 3L, 4L, 5L, 1L, 2L, 3L, 1L, 2L, 3L, 4L, 5L), 
  simsum = c(1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 2, 2)
)


x <- data.table(a = c(1,1,2,2), b = c(1,2,1,2))
y <- data.table(a = c(3,3,2,2), b = c(1,2,1,2))
x$a[1] <- NA

pairs <- pair_minsim(x, y, on = c("b", "a"), minsim = 1)
expect_equal(pairs$.x, c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L))
expect_equal(pairs$.y, c(1L, 3L, 2L, 4L, 1L, 3L, 4L, 2L, 3L, 4L))
expect_equal(pairs$simsum, c(1, 1, 1, 1, 1, 2, 1, 1, 1, 2))

dput(pairs)

data.table(
  .x = c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L), 
  .y = c(1L, 3L, 2L, 4L, 1L, 3L, 4L, 2L, 3L, 4L), 
  simsum = c(1, 1, 1, 1, 1, 2, 1, 1, 1, 2))

res <- pair_minsim(linkexample1, linkexample2, on = c("postcode", "lastname"), minsim = 0.5)


pairs1 <- pair_blocking(linkexample1, linkexample2, on = "postcode")
pairs2 <- pair_blocking(linkexample1, linkexample2, on = "lastname")


if (!isTRUE(all.equal(attr(pairs1, "x"), attr(pairs2, "x")))) 
  stop("The dataset x of the first set of pairs is not equal to the dataset ",
    "x of the second set.")
if (!isTRUE(all.equal(attr(pairs1, "y"), attr(pairs2, "y")))) 
  stop("The dataset x of the first set of pairs is not equal to the dataset ",
    "x of the second set.")
res <- rbind(pairs1, pairs2)
setkey(res, .x, .y)
res <- unique(res)
res <- structure(res, class = c("pairs", class(res)), x = attr(pairs1, "x"), 
  y = attr(pairs1, "y"))






row.names = c(NA, -15L), class = c("pairs", 
"data.table", "data.frame"), .internal.selfref = <pointer: 0x7fffe9692240>, x = structure(list(
    id = 1:6, lastname = structure(c(3L, 3L, 1L, 1L, 1L, 2L), levels = c("Johnson", 
    "Schwartz", "Smith"), class = "factor"), firstname = structure(c(1L, 
    5L, 1L, 3L, 4L, 2L), levels = c("Anna", "Ben", "Charles", 
    "Charly", "George"), class = "factor"), address = structure(c(2L, 
    2L, 3L, 3L, 3L, 1L), levels = c("1 Eaststr", "12 Mainstr", 
    "61 Mainstr"), class = "factor"), sex = structure(c(1L, 2L, 
    1L, 2L, 2L, 2L), levels = c("F", "M"), class = "factor"), 
    postcode = structure(c(NA, 1L, 2L, 1L, 1L, 2L), levels = c("1234 AB", 
    "6789 XY"), class = "factor")), row.names = c(NA, -6L), class = c("data.table", 
"data.frame"), .internal.selfref = <pointer: 0x7fffe9692240>), y = structure(list(
    id = c(2L, 3L, 4L, 6L, 7L), lastname = structure(c(4L, 2L, 
    1L, 3L, 3L), levels = c("Johnson", "Jonson", "Schwartz", 
    "Smith"), class = "factor"), firstname = structure(c(5L, 
    1L, 4L, 3L, 2L), levels = c("A.", "Anna", "Ben", "Charles", 
    "Gearge"), class = "factor"), address = structure(c(3L, 5L, 
    4L, 2L, 1L), levels = c("1 Eaststr", "1 Main", "12 Mainstreet", 
    "61 Mainstr", "61 Mainstreet"), class = "factor"), sex = structure(c(NA, 
    1L, 1L, 2L, 1L), levels = c("F", "M"), class = "factor"), 
    postcode = structure(c(1L, 1L, 1L, 2L, 2L), levels = c("1234 AB", 
    "6789 XY"), class = "factor")), row.names = c(NA, -5L), class = c("data.table", 
"data.frame"), .internal.selfref = <pointer: 0x7fffe9692240>))


pairs <- unique(rbindlist(list(pair_blocking(dfA, dfB, "postcode"), pair_blocking(dfA, dfB, "birthyear"))))
setattr(pairs, "class", c("pairs", class(pairs))) #add the 'pairs' class
setattr(pairs, "blocking_on", "postcode OR birthyear") #add info on blocking variables for the print function
setattr(pairs, "x", as.data.table(dfA)) #add first dataframe's data as x
setattr(pairs, "y", as.data.table(dfB)) #add second dataframe's data as y

