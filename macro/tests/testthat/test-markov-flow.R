test_that("markov_flow initializes", {
  patch_count <- 10
  parameters <- list(
    # Equal probability for any patch.
    flow_probability = matrix(data = 0.03, nrow = patch_count, ncol = patch_count),
    human_count = 100,
    path_count = patch_count
  )
  # Round-robin placement of humans.
  initial_state <- list(
    human_locations = rep(1:patch_count, ceiling(parameters$human_count / patch_count))[1:parameters$human_count]
  )
  movement <- movement_init(parameters, human_locations)
  expect_false(is.null(movement))
})
