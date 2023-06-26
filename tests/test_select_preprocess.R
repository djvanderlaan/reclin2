library(reclin2)
source("helpers.R")


data("linkexample1", "linkexample2")
linkexample1$id2 <- c(1,1,3,3,5,6)
linkexample2$id2 <- c(2,3,3,6,7)

pairs <- pair_blocking(linkexample1, linkexample2, "postcode")
pairs[, score := runif(nrow(pairs))]
pairs[, select := score > 0.5]
pairs[, select2 := FALSE]


# === TESTS FOR ID_X AND ID_Y ARGUMENTS

test <- reclin2:::select_preprocess(pairs, score = "score", id_x = "id", id_y = "id")
expect_equal(test$.x, linkexample1$id[pairs$.x])
expect_equal(test$.y, linkexample2$id[pairs$.y])

test <- reclin2:::select_preprocess(pairs, score = "score", id_x = "id2", id_y = "id2")
expect_equal(test$.x, linkexample1$id2[pairs$.x])
expect_equal(test$.y, linkexample2$id2[pairs$.y])

test <- reclin2:::select_preprocess(pairs, score = "score", id_x = "id2", id_y = "id2",
  preselect = "select")
expect_equal(sort(test$.x), sort(linkexample1$id2[pairs$.x[pairs$select]]))
expect_equal(sort(test$.y), sort(linkexample2$id2[pairs$.y[pairs$select]]))

test <- reclin2:::select_preprocess(pairs, score = "score", id_x = "id2", id_y = "id2",
  preselect = "select2")
expect_equal(nrow(test), 0)
expect_equal(names(test), c(".x", ".y", "score", "index"))

expect_error(
  test <- reclin2:::select_preprocess(pairs, score = "score", id_x = 1:3, id_y = 1:3,
    preselect = "select")
)
