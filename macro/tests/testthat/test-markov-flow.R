test_that("markov_flow initializes", {
  parameters <- list()
  movement <- movement_init(parameters)
  expect_false(is.null(movement))
})
