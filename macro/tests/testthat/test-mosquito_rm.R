library(data.table)

test_that("look_back_eip is right for small problem", {
  ll <- look_back_eip(c(3,3,2,2,2))
  compare <- list(c(2), c(2), numeric(0), c(3), c(3, 2))
  for (i in 1:5) {
    expect_equal(ll[[i]], compare[[i]])
  }
})


day_pairs <- list(
  c(1, 1),
  c(365, 365),
  c(366, 1),
  c(730, 365),
  c(731, 1)
)
for (day_pair in day_pairs) {
  test_that("day_within_year is right for 1", {
    expect_equal(day_within_year(day_pair[1]), day_pair[2])
  })
}


test_that("shift interval goes right and accumulates", {
  aa <- matrix(
    c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10), nrow = 2
    )
  bb <- aa %*% shift_with_open_interval(5)
  answer <- matrix(
    c(0, 0, 1, 6, 2, 7, 3, 8, 9, 19), nrow = 2
    )
  expect_equal(bb, answer)
})


test_that("mosquito-rm averaging of kappa runs", {
  patch_cnt <- 4
  duration <- 10
  kappa <- matrix(rep(0.2, patch_cnt * duration), nrow = patch_cnt)
  averaged <- mosquito_rm_average_kappa(kappa)
  expect_equal(dim(averaged)[1], patch_cnt)
  expect_equal(dim(averaged)[2], duration)
  expect_true(all(abs(averaged - 0.2) < 1e-14))
})


# Maternal population should be stable when aquatic stage gives
# (1-p)M new adults.
test_that("mosquito-rm stable initialization is completely correct", {
  patch_cnt <- 5
  params <- build_biting_parameters(patch_cnt)
  internal_params <- build_internal_parameters(params)
  state <- mosquito_rm_build_biting_state(params)

  # Make a deterministic aquatic function.
  aquatic <- function(lambda) lambda

  b <- 0.011111108632842
  bites_patch = b * (1 - params$infected_fraction) * 900
  initial_state <- mosquito_rm_copy_state(state)
  for (i in 1:1000) {
    state <- mosquito_rm_dynamics(state, internal_params, bites_patch, aquatic)
  }
  expect_true(macro:::within_absolute_error(
    state$M[1], initial_state$M[1], 1e-5))
  expect_true(macro:::within_absolute_error(
    state$Y[1, 1], initial_state$Y[1, 1], 1e-5))
})


# The sum of the Y values should be Y0 / (1 - p)
test_that("mosquito-rm aggregates Y0 geometrically", {
  patch_cnt <- 5
  params <- build_biting_parameters(patch_cnt)
  internal_params <- build_internal_parameters(params)
  state <- mosquito_rm_build_biting_state(params)

  aquatic <- function(lambda) lambda

  b <- 0.011111108632842
  bites_patch = b * (1 - params$infected_fraction) * 900
  initial_state <- mosquito_rm_copy_state(state)
  for (i in 1:100) {
    state <- mosquito_rm_dynamics(state, internal_params, bites_patch, aquatic)
  }
  expect_true(macro:::within_absolute_error(
    state$Y[, 1] / (1 - params$p), rowSums(state$Y) + state$Z, 1e-5))
})


test_that("mosquito-rm lag is correct for EIP", {
  patch_cnt <- 5
  params <- build_biting_parameters(patch_cnt)
  params$EIP[1:49] <- 12
  params$EIP[50:365] <- 10
  internal_params <- build_internal_parameters(params)
  state <- mosquito_rm_build_biting_state(params)
  aquatic <- function(lambda) lambda
  b <- 0.011111108632842
  initial_state <- mosquito_rm_copy_state(state)
  bites_patch = b * (1 - params$infected_fraction) * 900
  iteration_cnt <- 100
  running_z <- numeric(iteration_cnt)
  for (i in 1:iteration_cnt) {
    state <- mosquito_rm_dynamics(state, internal_params, bites_patch, aquatic)
    running_z[i] <- state$Z[1]
  }
  zmean <- mean(running_z[40:50])
  zsd <- sd(running_z[40:50])
  different <- which(running_z[50:365] > zmean + 5 * zsd)
  expect_true(different[1] == 10)
})


test_that("mosquito-rm converts incoming kappa to internal shape", {
  patch_cnt <- 5
  duration <- 10
  kappa_dt <- sample_mosquito_kappa(patch_cnt, duration)
  kappa <- macro:::mosquito_rm_convert_bloodmeal(kappa_dt)
  expect_equal(dim(kappa)[1], patch_cnt)
  expect_equal(dim(kappa)[2], duration)
})


test_that("mosquito-rm module internals can do a ten-day time step", {
  patch_cnt <- 5L
  user_parameters <- build_biting_parameters(patch_cnt)
  module <- mosquito_rm_module(user_parameters)
  past_kappa <- matrix(rep(0.2, patch_cnt * user_parameters$duration), nrow = patch_cnt)
  chunk_step <- mosquito_rm_discrete_step(module, past_kappa)
  expect_equal(names(module$state), names(chunk_step$state))
})


test_that("mosquito-rm module can do a time step", {
  patch_cnt <- 5L
  user_parameters <- build_biting_parameters(patch_cnt)
  module_initial <- mosquito_rm_module(user_parameters)
  bloodmeal_dt <- sample_mosquito_kappa(patch_cnt, user_parameters$duration)
  time_id <- list(time = 0, duration = 10)
  module <- mash_step(module_initial, time_id, bloodmeal_dt)
  expect_equal(names(module_initial), names(module))
  result <- module$output
  expect_equal(nrow(result), patch_cnt * user_parameters$duration)
  expect_true(all(result$M >= 0))
  expect_true(all(result$Y >= 0))
  expect_true(all(result$Z >= 0))
})


test_that("mosquito-rm module can observe results", {
  patch_cnt <- 5L
  user_parameters <- build_biting_parameters(patch_cnt)
  module <- mosquito_rm_module(user_parameters)
  bloodmeal_dt <- data.table::data.table(expand.grid(1:patch_cnt, 1:user_parameters$duration))
  names(bloodmeal_dt) <- c("Location", "Time")
  bloodmeal_dt$Time <- bloodmeal_dt$Time - 1
  bloodmeal_dt$Bites <- sample(10:20, nrow(bloodmeal_dt), replace = TRUE)
  time_id <- list(time = 0, duration = 10)
  module <- mash_step(module, time_id, bloodmeal_dt)
  bites_dt <- mosquito_path(module)
  expect_equal(nrow(bites_dt), patch_cnt * user_parameters$duration)
})
