test_that("can use data.table", {
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
  data.table::setkey(individuals, when)

  trajectory <- continuous_step(individuals, is_enabled, when, fire)
})
