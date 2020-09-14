test_that("look_back_eip is right for small problem", {
  ll <- look_back_eip(c(3,3,2,2,2))
  compare <- list(c(2), c(2), numeric(0), c(3), c(3, 2))
  for (i in 1:5) {
    expect_equal(ll[[i]], compare[[i]])
  }
})


test_that("day_within_year is right for 1", {
  expect_equal(day_within_year(1), 1)
})


test_that("day_within_year is right for 365", {
  expect_equal(day_within_year(365), 365)
})


test_that("day_within_year is right for 366", {
  expect_equal(day_within_year(366), 1)
})


test_that("day_within_year is right for 730", {
  expect_equal(day_within_year(730), 365)
})

test_that("day_within_year is right for 731", {
  expect_equal(day_within_year(731), 1)
})


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


# Maternal population should be stable when aquatic stage gives
# (1-p)M new adults.
test_that("mosquito-rm maternal stable population is stable", {
  patch_cnt <- 5
  params <- build_biting_parameters(patch_cnt)
  internal_params <- build_internal_parameters(params)
  infectious <- rep(0.4, patch_cnt)
  mortality <- 1 - params$p
  state <- build_biting_state(infectious, mortality, params$maxEIP)

  replacement <- (1 -params$p) / params$p
  aquatic <- function(lambda) rep(replacement, length(lambda))

  kappa <- 0.2
  kappa_patch = rep(kappa, patch_cnt)
  initial_state <- mrm_copy_state(state)
  for (i in 1:10) {
    state <- mosquito_rm_dynamics(state, internal_params, kappa_patch, aquatic)
  }
  expect_true(within_absolute_error(state$M, initial_state$M, 1e-5))
})


# The sum of the Y values should be Y0 / (1 - p)
test_that("mosquito-rm aggregates Y0 geometrically", {
  patch_cnt <- 5
  params <- build_biting_parameters(patch_cnt)
  internal_params <- build_internal_parameters(params)
  infectious <- rep(0.4, patch_cnt)
  mortality <- 1 - params$p
  state <- build_biting_state(infectious, mortality, params$maxEIP)

  replacement <- (1 -params$p) / params$p
  aquatic <- function(lambda) rep(replacement, length(lambda))

  kappa <- 0.2
  kappa_patch = rep(kappa, patch_cnt)
  initial_state <- mrm_copy_state(state)
  for (i in 1:100) {
    state <- mosquito_rm_dynamics(state, internal_params, kappa_patch, aquatic)
  }
  expect_true(within_absolute_error(state$Y[, 1] / (1 - params$p), rowSums(state$Y), 1e-5))
})


# When the maternal population is stable, the incubating population will
# converge to a kappa (p - 1) / (p (1 + a kappa) - 1)
test_that("mosquito-rm incubating is stable", {
  patch_cnt <- 5
  params <- build_biting_parameters(patch_cnt)
  internal_params <- build_internal_parameters(params)
  infectious <- rep(0.4, patch_cnt)
  mortality <- 1 - params$p
  state <- build_biting_state(infectious, mortality, params$maxEIP)

  replacement <- (1 -params$p) / params$p
  aquatic <- function(lambda) rep(replacement, length(lambda))

  kappa <- 0.2
  kappa_patch = rep(kappa, patch_cnt)
  initial_state <- mrm_copy_state(state)
  for (i in 1:100) {
    state <- mosquito_rm_dynamics(state, internal_params, kappa_patch, aquatic)
  }
  y0_expected <- with(params,
    rep(a * kappa / (1 + a * kappa * p / (1 - p)), patch_cnt))
  expect_true(within_absolute_error(state$Y[, 1], y0_expected, 1e-5))
})


# When the maternal population is stable, the incubating population will
# converge to a kappa (p - 1) / (p (1 + a kappa) - 1)
test_that("mosquito-rm infectious is stable", {
  patch_cnt <- 5
  params <- build_biting_parameters(patch_cnt)
  internal_params <- build_internal_parameters(params)
  infectious <- rep(0.4, patch_cnt)
  mortality <- 1 - params$p
  state <- build_biting_state(infectious, mortality, params$maxEIP)

  replacement <- (1 -params$p) / params$p
  aquatic <- function(lambda) rep(replacement, length(lambda))

  kappa <- 0.2
  kappa_patch = rep(kappa, patch_cnt)
  initial_state <- mrm_copy_state(state)
  for (i in 1:100) {
    state <- mosquito_rm_dynamics(state, internal_params, kappa_patch, aquatic)
  }
  y0_expected <- with(params, a * kappa / (1 + a * kappa * p / (1 - p)))
  eip = params$EIP[[1]]
  p = params$p
  z_expected <- rep(p^eip * y0_expected / (1 - p), patch_cnt)
  expect_true(within_absolute_error(state$Z, z_expected, 1e-4))
})


test_that("mosquito-rm lag is correct for EIP", {
  patch_cnt <- 5
  params <- build_biting_parameters(patch_cnt)
  params$EIP[1:49] <- 12
  params$EIP[50:365] <- 10
  internal_params <- build_internal_parameters(params)
  infectious <- rep(0.4, patch_cnt)
  mortality <- 1 - params$p
  state <- build_biting_state(infectious, mortality, params$maxEIP)

  replacement <- (1 -params$p) / params$p
  aquatic <- function(lambda) rep(replacement, length(lambda))

  kappa <- 0.2
  kappa_patch = rep(kappa, patch_cnt)
  initial_state <- mrm_copy_state(state)
  iteration_cnt <- 100
  running_z <- numeric(iteration_cnt)
  for (i in 1:iteration_cnt) {
    state <- mosquito_rm_dynamics(state, internal_params, kappa_patch, aquatic)
    running_z[i] <- state$Z[1]
  }
  zmean <- mean(running_z[40:50])
  zsd <- sd(running_z[40:50])
  different <- which(running_z[50:365] > zmean + 5 * zsd)
  expect_true(different[1] == 10)
})
