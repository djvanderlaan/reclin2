

# Set the seed and return the old seed. This can be used to restore the seed
# when setting the seed in a function. Example:
#
# foo <- function(...) {
#   oldseed <- set_seed(1)
#   on.exit(set_seed(oldseed))
# }
# 
set_seed <- function(seed, ...) {
  if(!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    set.seed(NULL)
  old_seed <- get(".Random.seed", envir = .GlobalEnv)
  if (length(seed) > 1) {
    assign(".Random.seed", seed, envir = .GlobalEnv)
  } else {
    set.seed(seed, ...)
  }
  invisible(old_seed)
}

