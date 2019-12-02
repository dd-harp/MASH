test_that("can make random seeds for Mersenne Twister", {
  skip_count <- 5
  mersenne_dimensions <- 624
  seed_matrix <- skip_mersenne_twister(4789, 17, skip_count)
  seed_dimensions <- dim(seed_matrix)
  expect_equal(seed_dimensions[1], mersenne_dimensions)
  expect_equal(seed_dimensions[2], skip_count)
})


# The generator uses unsigned integers for initialization,
# but R represents them as signed integers. Ensure some
# of the 624 integers are less than zero.
test_that("random seeds are over 31 bits", {
  seed_matrix <- skip_mersenne_twister(4789, 17, 10)
  expect_gt(31, log(max(abs(seed_matrix)), 2))
})


test_that("can use random seeds to set .Random.seed", {
  seed_matrix <- skip_mersenne_twister(4789, 17, 2)
  set.seed(23234)
  set_mersenne_seed(seed_matrix[, 1])
  a <- runif(10)
  set_mersenne_seed(seed_matrix[, 2])
  b <- runif(10)
  expect_equal(0, sum(a == b))
  set_mersenne_seed(seed_matrix[, 1])
  c <- runif(10)
  expect_equal(a, c)
})
