#' Set the seed on the Mersenne Twister random generator.
#'
#' The input is a vector of 624 integers, used to initialize
#' an exact state for MT19937. We assume it comes from
#'
#' copies <- skip_mersenne_twister(4789, 17, 100)
#' this_run <- 1 # Out of 100 copies we run.
#' set_mersenne_seed(copies[, this_run])
set_mersenne_seed <- function(seed_vector) {
  RNGkind(kind="Mersenne-Twister")
  offset <- length(.Random.seed) - length(seed_vector)
  .Random.seed[(1 + offset):(offset + length(seed_vector))] <<-
    seed_vector
  invisible(NULL)
}
