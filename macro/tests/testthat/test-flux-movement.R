test_that("markov_flow initializes", {
  patch_count <- 10
  human_count <- 100
  parameters <- list(
    # Equal probability for any patch.
    flow_probability = matrix(data = 0.03, nrow = patch_count, ncol = patch_count),
    human_count = human_count,
    patch_count = patch_count
  )
  # Round-robin placement of humans.
  initial_state <- list(
    human_locations = rep(1:patch_count, ceiling(human_count / patch_count))[1:human_count]
  )
  movement <- flux_movement_init(parameters, initial_state)
  expect_false(is.null(movement))
})
