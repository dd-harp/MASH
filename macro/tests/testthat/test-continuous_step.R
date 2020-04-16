test_that("individual two state bounce", {
  set.seed(32432)
  is_enabled <- list(
    infect = function(state) with(state, disease == "S"),
    recover = function(state) with(state, disease == "I")
  )
  when <- list(
    infect = function(state) rexp(1, 1/50),
    recover = function(state) rexp(1, 1/200)
  )
  fire <- list(
    infect = function(state, time) {within(state, {disease = "I"})},
    recover = function(state, time) {within(state, {disease = "S"})}
  )

  individual <- list(disease = "S", when = 0.32, infect = 0.32, recover = Inf)
  status <- update_individual(individual, is_enabled, when, fire)$individual
  expect_equal(status$disease, "I")
  expect_equal(status$infect, Inf)
  expect_gt(status$recover, 0.32)
  expect_equal(status$when, status$recover)

  individual <- list(disease = "I", when = 0.44, infect = Inf, recover = 0.44)
  status <- update_individual(individual, is_enabled, when, fire)$individual
  expect_equal(status$disease, "S")
  expect_equal(status$recover, Inf)
  expect_gt(status$infect, 0.44)
  expect_equal(status$when, status$infect)
})


#' The individual is in an absorbing state, so there is nothing
#' else to fire. Check that it goes to Inf for the next time.
test_that("individual removed goes to inf", {
  set.seed(32432)
  is_enabled <- list(
    infect = function(state) with(state, disease == "S")
  )
  when <- list(
    infect = function(state) rexp(1, 1/50)
  )
  fire <- list(
    infect = function(state, time) {within(state, {disease = "I"})}
  )

  individual <- list(disease = "S", when = 0.32, infect = 0.32)
  # browser()
  status <- update_individual(individual, is_enabled, when, fire)$individual
  expect_equal(status$disease, "I")
  expect_equal(status$infect, Inf)
  expect_equal(status$when, Inf)
})


#' A transition was enabled and fired, but it's now enabled again
#' in the new state. Check that it gets scheduled again.
test_that("individual transition fires again", {
  set.seed(32432)
  is_enabled <- list(
    infect = function(state) with(state, disease == "S")
  )
  when <- list(
    infect = function(state) rexp(1, 1/50)
  )
  # Returns to same state when it fires.
  fire <- list(
    infect = function(state, time) {within(state, {disease = "S"})}
  )

  individual <- list(disease = "S", when = 0.32, infect = 0.32)
  # browser()
  status <- update_individual(individual, is_enabled, when, fire)$individual
  expect_equal(status$disease, "S")
  expect_gt(status$infect, 0.32)
  expect_equal(status$when, status$infect)
})


test_that("can use data.table", {
  set.seed(97234)
  # This sets up the transitions.
  program_globals <- new.env()
  program_globals$b <- 0.2
  program_globals$c <- 3.7

  is_enabled <- list(
    infect = function(state) with(state, disease == "S"),
    recover = function(state) with(state, disease == "I")
  )
  when <- list(
    infect = function(state) rexp(1, 1/50),
    recover = function(state) rexp(1, 1/200)
  )
  fire <- list(
    infect = function(state, time) {
      b <<- 0.3
      within(state, {disease = "I"})
    },
    recover = function(state, time) {
      b <<- 0.25
      within(state, {disease = "S"})
    }
  )
  # Setting the environment of a function gives it access to the environment
  # through the <<- assignment operator.
  for (set_env_idx in 1:length(fire)) {
    environment(fire[[set_env_idx]]) <- program_globals
  }

  # This sets up the data.table of individuals.
  individuals <- data.table::data.table(
    disease = factor(c("S", "I", "S"), levels = c("S", "I")),
    when = rep(Inf, 3),
    infect = rep(Inf, 3),
    recover = rep(Inf, 3)
  )
  # Setting the key makes it much faster to find the soonest time.
  data.table::setkey(individuals, when)

  # Test initialization.
  initialized <- initialize_times(individuals, is_enabled, when)
  # Check that, for this model, each individual has at least one transition initialized.
  expect_true(all(is.finite(initialized[, infect]) | is.finite(initialized[, recover])))
  trajectory <- continuous_step(individuals, is_enabled, when, fire)
  expect_equal(length(trajectory), 2 * 5)
})
