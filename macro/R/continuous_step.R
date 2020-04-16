#' Set up firing times given an initial state.
#'
#' @param individuals a data frame of individuals, including columns for times.
#' @param is_enabled a list of functions that say when transitions are enabled
#' @param when a list of functions that return times when enabled
#' @return a data frame of individuals, with times filled in
#' @export
initialize_times <- function(individuals, is_enabled, when) {
  for (person_idx in 1:nrow(individuals)) {
    state <- as.list(individuals[person_idx])
    newly_enabled <- vapply(
      is_enabled,
      FUN = function(x) x(state),
      FUN.VALUE = vector(mode = "logical", length = 1))
    new_when <- vapply(
      when[newly_enabled],
      FUN = function(x) x(state),
      FUN.VALUE = vector(mode = "numeric", length = 1))
    state[names(new_when)] <- new_when
    state["when"] <- min(new_when)
    individuals[person_idx, ] <- state
  }
  individuals
}

#' Update one individual
#'
#' @param state a list of their traits and times
#' @param is_enabled a list of functions to determine enabling
#' @param when a list of functions to determine when to fire
#' @param fire a list of functions to determine firing
#' @return a list with the individual a trajectory entry, and a new time
#' @export
update_individual <- function(state, is_enabled, when, fire) {
  current_time <- state$when
  # Select the transition times (excluding the minimum time "when")
  # We assume that the lists of functions are all in the same order.
  when_col <- length(state) - length(fire)
  times <- unlist(state[(when_col + 1):length(state)])

  # Fire!
  to_fire <- which.min(times)
  trajectory <- list(event = names(to_fire), when = current_time)
  new_state <- fire[[to_fire]](state, current_time)
  times[to_fire] <- Inf  # mark the one that fired as not being enabled so it can fire next.

  # Turn transitions on/off as a result of firing.
  was_enabled <- is.finite(times)
  enabled <- vapply(
    is_enabled,
    FUN = function(x) x(new_state),
    FUN.VALUE = vector(mode = "logical", length = 1))
  times[!enabled] <- Inf  # disable competing transitions

  # Compute times for newly-enabled transitions
  newly_enabled <- !was_enabled & enabled
  if (any(newly_enabled)) {
    new_times <- current_time +
      vapply(
        when[newly_enabled],
        FUN = function(x) x(new_state),
        FUN.VALUE = vector(mode = "numeric", length = 1))
    times[newly_enabled] <- new_times
  }
  new_state[when_col:length(new_state)] <- c(min(times), times)
  list(individual = new_state, trajectory = trajectory, time = current_time)
}


#' Check preconditions for the data structures.
#' @param individuals a data frame of individuals, including columns for times.
#' @param is_enabled a list of enabling functions.
#' @param when a list of functions that return firing times when enabled
#' @param fire a list of functions that say when to fire.
check_continuous_setup <- function(individuals, is_enabled, when, fire) {
  stopifnot(all(names(is_enabled) == names(when)))
  stopifnot(all(names(is_enabled) == names(fire)))
  when_col <- length(individuals) - length(is_enabled)
  stopifnot(names(individuals)[when_col] == "when")
  stopifnot(all(names(individuals)[(when_col + 1):length(individuals)] == names(is_enabled)))
}


#' Run until a specified time.
#'
#' @param individuals a data frame of individuals, including columns for times.
#' @param is_enabled a list of enabling functions.
#' @param when a list of functions that return firing times when enabled
#' @param fire a list of functions that say when to fire.
#' @return a trajectory
#' @export
continuous_step <- function(individuals, is_enabled, when, fire) {
  check_continuous_setup(individuals, is_enabled, when, fire)
  # The current time and next firing times are part of the next state of the system.
  # If a firing time is Inf, that means it isn't scheduled.
  individuals <- initialize_times(individuals, is_enabled, when)

  step_cnt <- 5
  trajectory <- vector(mode = "list", length = step_cnt)
  for (step_idx in 1:step_cnt) {
    individual <- as.list(individuals[order(when)][1])
    if (is.infinite(individual$when)) break
    new_state <- update_individual(individual, is_enabled, when, fire)
    trajectory[[step_idx]] <- new_state$trajectory
    individuals[order(when)][1] <- new_state$individual
  }
  do.call(rbind, trajectory)
}
