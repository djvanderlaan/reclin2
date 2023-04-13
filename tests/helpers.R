expect_equal <- function(x, y, attributes = TRUE) {
  if (is.data.frame(x) && !attributes) 
    return(expect_equal_data.frame(x, y, attributes))
  if (!attributes) attributes(x) <- NULL
  if (!attributes) attributes(y) <- NULL
  stopifnot(isTRUE(all.equal(x, y)))
}

expect_equal_data.frame <- function(x, y, attributes = TRUE) {
  if (!attributes) {
    expect_equal(class(x), class(y))
    expect_equal(names(x), names(y))
    for (col in names(x)) 
      expect_equal(x[[col]], y[[col]], attributes = FALSE)
  } else expect_equal(x, y, attributes)
}

expect_attribute <- function(x, name, value) {
  v <- attr(x, name)
  stopifnot(!is.null(v))
  expect_equal(v, value)
}

expect_error <- function(expr) {
  expect_error.error <- TRUE
  try({
    expr
    expect_error.error <- FALSE
  }, silent = TRUE)
  if (!expect_error.error) stop("Expression did not throw an error.")
}

expect_warning <- function(expr) {
  messages <- list()
  warnings <- list()  
  errors   <- list()
  #tryCatch(
    withCallingHandlers(
      expr, 
      warning = function(w) { 
        warnings <<- append(warnings, list(w))
        invokeRestart("muffleWarning")
      }#,
      #message = function(m) {
      #  messages <<- append(messages, list(m))
      #  invokeRestart("muffleMessage")
      #}
    )#, 
    #error  = function(e) errors <<- append(errors, list(e))
  #)
  stopifnot(length(warnings) > 0)
}
 
