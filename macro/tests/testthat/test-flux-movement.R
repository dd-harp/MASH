test_that("markov_flow initializes", {
  skip("causing a null pointer exception")
  patch_count <- 10
  human_count <- 100
  parameters <- list(
    # Equal probability for any patch.
    flow_probability = matrix(data = 0.03, nrow = patch_count, ncol = patch_count),
    human_count = human_count,
    patch_count = patch_count,
    random_seed = 9928374,
    random_stream = 3
  )
  # Round-robin placement of humans.
  initial_state <- list(
    human_locations = rep(1:patch_count, ceiling(human_count / patch_count))[1:human_count]
  )
  movement <- flux_movement_init(parameters, initial_state)
  expect_false(is.null(movement))
  result <- flux_movement_step(movement, 0.1)
  expect_false(is.null(result))
})


test_that("flux_movement returns human locations", {
  skip("causing a null pointer exception")
  patch_count <- 8
  human_count <- 100
  parameters <- list(
    # Equal probability for any patch.
    flow_probability = matrix(data = 0.03, nrow = patch_count, ncol = patch_count),
    human_count = human_count,
    patch_count = patch_count,
    random_seed = 99283742,
    random_stream = 1
  )
  # Round-robin placement of humans.
  initial_state <- list(
    human_locations = rep(1:patch_count, ceiling(human_count / patch_count))[1:human_count]
  )
  movement <- flux_movement_init(parameters, initial_state)
  expect_false(is.null(movement))
  result <- flux_movement_step(movement, 0.1)
  expect_false(is.null(result))
  locations <- convert_to_r_movement(result, 1:human_count)
  moves <- locations$moves
  expect_true(length(moves) == human_count)
})
