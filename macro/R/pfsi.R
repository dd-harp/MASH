pfsi_transitions <- function(params, simulation_state) {
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
    environment(fire[[set_env_idx]]) <- simulation_state
  }

  list(is_enabled = is_enabled, when = when, fire = fire)
}


construct_pfsi <- function() {
  program_globals <- new.env()
  program_globals$b <- 0.2
  program_globals$c <- 3.7

  # This sets up the data.table of individuals.
  individuals <- data.table::data.table(
    disease = factor(c("S", "I", "S"), levels = c("S", "I")),
  )
  # Setting the key makes it much faster to find the soonest time.
  data.table::setkey(individuals, when)

  params <- list()
  transitions <- pfsi_transitions(params, program_globals)
}
