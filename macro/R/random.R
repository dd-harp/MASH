set_mersenne_seed <- function(seed_vector) {
  RNGkind(kind="Mersenne-Twister")
  offset <- length(.Random.seed) - length(seed_vector)
  .Random.seed[(1 + offset):(offset + length(seed_vector))] <<-
    seed_vector
}
