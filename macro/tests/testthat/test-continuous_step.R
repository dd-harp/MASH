test_that("individual two state bounce", {
  set.seed(32432)
  is_enabled <- list(
    infect = function(state, time) with(state, disease == "S"),
    recover = function(state, time) with(state, disease == "I")
  )
  when <- list(
    infect = function(state, time) rexp(1, 1/50),
    recover = function(state, time) rexp(1, 1/200)
  )
  fire <- list(
    infect = function(state, time) {within(state, {disease = "I"})},
    recover = function(state, time) {within(state, {disease = "S"})}
  )
  transitions <- list(
    is_enabled = is_enabled, when = when, fire = fire
  )

  individual <- list(disease = "S", when = 0.32, infect = 0.32, recover = Inf)
  status <- update_individual(individual, transitions, observe_continuous)$individual
  expect_equal(status$disease, "I")
  expect_equal(status$infect, Inf)
  expect_gt(status$recover, 0.32)
  expect_equal(status$when, status$recover)

  individual <- list(disease = "I", when = 0.44, infect = Inf, recover = 0.44)
  status <- update_individual(individual, transitions, observe_continuous)$individual
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
    infect = function(state, time) with(state, disease == "S")
  )
  when <- list(
    infect = function(state, time) rexp(1, 1/50)
  )
  fire <- list(
    infect = function(state, time) {within(state, {disease = "I"})}
  )
  transitions <- list(
    is_enabled = is_enabled, when = when, fire = fire
  )
  individual <- list(disease = "S", when = 0.32, infect = 0.32)
  # browser()
  status <- update_individual(individual, transitions, observe_continuous)$individual
  expect_equal(status$disease, "I")
  expect_equal(status$infect, Inf)
  expect_equal(status$when, Inf)
})


#' A transition was enabled and fired, but it's now enabled again
#' in the new state. Check that it gets scheduled again.
test_that("individual transition fires again", {
  set.seed(32432)
  is_enabled <- list(
    infect = function(state, time) with(state, disease == "S")
  )
  when <- list(
    infect = function(state, time) rexp(1, 1/50)
  )
  # Returns to same state when it fires.
  fire <- list(
    infect = function(state, time) {within(state, {disease = "S"})}
  )
  transitions <- list(
    is_enabled = is_enabled, when = when, fire = fire
  )
  individual <- list(disease = "S", when = 0.32, infect = 0.32)
  # browser()
  status <- update_individual(individual, transitions, observe_continuous)$individual
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

  transitions <- list(
    infect = list(
      is_enabled = function(state, time) with(state, disease == "S"),
      when = function(state, time) rexp(1, 1/50),
      fire = function(state, time) {
        b <<- 0.3
        within(state, {disease = "I"})
      }
    ),
    recover = list(
      is_enabled = function(state, time) with(state, disease == "I"),
      when = function(state, time) rexp(1, 1/200),
      fire = function(state, time) {
        b <<- 0.25
        within(state, {disease = "S"})
      }
    )
  )

  # This sets up the data.table of individuals.
  individuals <- data.table::data.table(
    disease = factor(c("S", "I", "S"), levels = c("S", "I"))
  )

  simulation <- continuous_simulation(
    individuals,
    transitions,
    observe_continuous,
    program_globals
  )
  simulation <- init_simulation(simulation)
  simulation <- run_simulation(simulation, 200)
  trajectory <- simulation$trajectory[1:simulation$trajectory_cnt]
  expect_gt(length(trajectory), 1)
  for (check_idx in 1:length(trajectory)) {
    expect_true(trajectory[[check_idx]]$name %in% c("infect", "recover"))
  }
})
