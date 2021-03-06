library(data.table)

#' Check to see if transition is enabled for this individual
#'
#' This function is for testing the transitions on data for a
#' single individual. It's a helper function. The running simulation
#' doesn't call this function.
#'
#' @param transition A list of transitions
#' @param individual A list with individual properties
#' @param when A time
#' @param variables Any global simulation state
#' @return logical whether transition can fire given this state
#' @seealso \code{\link{when}}, \code{\link{fire}}
#' @export
is_enabled <- function(transition, individual, when, variables = NULL) {
  transition_names <- names(transition)
  enable_function <- ifelse(
    is.null(transition_names), transition[[1]], transition[["is_enabled"]])
  if (!is.null(variables)) {
    environment(enable_function) <- variables
  }  # else don't need to create an environment
  enable_function(individual, when)
}


#' Find when this transition will fire.
#'
#' This function is for testing the transitions on data for a
#' single individual. It's a helper function. The running simulation
#' doesn't call this function.
#'
#' @param transition A list of transitions
#' @param individual A list with individual properties
#' @param when A time
#' @param variables Any global simulation state
#' @return numeric time relative to now, when it would fire
#' @seealso \code{\link{is_enabled}}, \code{\link{fire}}
#' @export
when <- function(transition, individual, when, variables = NULL) {
  transition_names <- names(transition)
  when_function <- ifelse(
    is.null(transition_names), transition[[2]], transition[["when"]])
  if (!is.null(variables)) {
    environment(when_function) <- variables
  }  # else don't need to create an environment
  when_function(individual, when)
}


#' Fire transition, changing the individual
#'
#' This function is for testing the transitions on data for a
#' single individual. It's a helper function. The running simulation
#' doesn't call this function.
#'
#' @param transition A list of transitions
#' @param individual A list with individual properties
#' @param when A time
#' @param variables Any global simulation state
#' @return a new list describing the individual
#' @seealso \code{\link{is_enabled}}, \code{\link{when}}
#' @export
fire <- function(transition, individual, when, variables = NULL) {
  transition_names <- names(transition)
  fire_function <- ifelse(is.null(transition_names),
                          transition[[3]], transition[["fire"]])
  if (!is.null(variables)) {
    environment(fire_function) <- variables
  }  # else don't need to create an environment
  fire_function(individual, when)
}


check_missing_when <- function(new_when) {
  if (any(is.na(new_when) | is.nan(new_when))) {
    find_na <- which(is.na(new_when) | is.nan(new_when))
    stop(paste(
      "transition", names(new_when)[which],
      "values na", is.na(new_when[which]), "nan", is.nan(new_when[which])))
  }
}


#' Set up firing times given an initial state.
#'
#' @param individuals a data frame of individuals, including columns for times.
#' @param transitions a list of transitions for enabling and firing times
#' @return a data frame of individuals, with times filled in
#' @export
initialize_times <- function(individuals, transitions, curtime) {
  is_enabled <- transitions$is_enabled
  when <- transitions$when
  for (person_idx in 1:nrow(individuals)) {
    state <- as.list(individuals[person_idx,])
    newly_enabled <- vapply(
      is_enabled,
      FUN = function(x) x(state, curtime),
      FUN.VALUE = vector(mode = "logical", length = 1))
    if (any(newly_enabled)) {
      new_when <- curtime + vapply(
        when[newly_enabled],
        FUN = function(x) x(state, curtime),
        FUN.VALUE = vector(mode = "numeric", length = 1))
      check_missing_when(new_when)
      state[names(new_when)] <- new_when
      state["when"] <- min(new_when)
      individuals[person_idx, ] <- state
    }  # else nothing enabled for this person.
  }
  individuals
}


#' Update one individual
#'
#' @param state a list of their traits and times
#' @param transitions a list of transitions for enabling and firing times
#' @param observe A function that records the transition.
#' @return a list with the individual a trajectory entry, and a new time
#' @export
update_individual <- function(state, transitions, observe) {
  is_enabled <- transitions$is_enabled
  when <- transitions$when
  fire <- transitions$fire
  firing_time <- state$when
  # Select the transition times (excluding the minimum time "when")
  # We assume that the lists of functions are all in the same order.
  when_col <- length(state) - length(fire)
  times <- unlist(state[(when_col + 1):length(state)])

  # Fire!
  to_fire <- which.min(times)
  stopifnot(is.finite(times[to_fire]))
  new_state <- fire[[to_fire]](state, firing_time)
  # mark the one that fired as not being enabled so it can fire next.
  times[to_fire] <- Inf
  trajectory_entry <- observe(names(to_fire), state, new_state, firing_time)

  # Turn transitions on/off as a result of firing.
  was_enabled <- is.finite(times)
  enabled <- vapply(
    is_enabled,
    FUN = function(x) x(new_state, firing_time),
    FUN.VALUE = vector(mode = "logical", length = 1))
  times[!enabled] <- Inf  # disable competing transitions

  # Compute times for newly-enabled transitions
  newly_enabled <- !was_enabled & enabled
  if (any(newly_enabled)) {
    new_times <- firing_time +
      vapply(
        when[newly_enabled],
        FUN = function(x) x(new_state, firing_time),
        FUN.VALUE = vector(mode = "numeric", length = 1))
    times[newly_enabled] <- new_times
    check_missing_when(new_times)
  }
  new_state[when_col:length(new_state)] <- c(min(times), times)
  list(individual = new_state, curtime = firing_time, entry = trajectory_entry)
}


#' Check preconditions for the data structures.
#' @param individuals a data frame of individuals, including columns for times.
#' @param transitions a list of transitions for enabling and firing times
check_continuous_setup <- function(individuals, transitions) {
  is_enabled <- transitions$is_enabled
  when <- transitions$when
  fire <- transitions$fire
  stopifnot(all(names(is_enabled) == names(when)))
  stopifnot(all(names(is_enabled) == names(fire)))
  when_col <- length(individuals) - length(is_enabled)
  stopifnot(names(individuals)[when_col] == "when")
  stopifnot(all(
    names(individuals)[(when_col + 1):length(individuals)] ==
      names(is_enabled)))
}


#' Create a continuous-time simulation for a dataframe of individuals.
#'
#' @param individuals a data.table with a row for each individual
#' @param transitions Each transition is a list of enabling rule, enabling time,
#'     and firing functions.
#' @param observer A function that examines each transition and stores
#'     information about it. For example, see \code{\link{observe_continuous}}
#' @param variables An environment containing global variables for this
#'     simulation. This can be NULL.
#' @return A simulation object, as a list.
#'
#' This defines a continuous simulation by specifying a) system state
#' b) a list of transitions, and c) an observer. The system state is
#' a dataframe where each row represents an individual. Each column is a
#' property of that individual. Each transition is a list containing three
#' functions which act on the state, `is_enabled` to say whether the transition
#' could fire given the current state, `when` to sample a time to fire, and
#' `fire`, which changes the state for that individual.
#'
#' The observer is a function called each time a transition fires.
#'
#' @export
continuous_simulation <- function(
  individuals, transitions, observer, variables = NULL
  ) {
  # The simulation needs lists of kinds of transitions,
  # so unwrap what we're given.
  internal_transitions <- list(
    is_enabled = as.list(setNames(names(transitions), names(transitions))),
    when = as.list(setNames(names(transitions), names(transitions))),
    fire = as.list(setNames(names(transitions), names(transitions)))
  )
  for (transition_name in names(transitions)) {
    transition_triple <- transitions[[transition_name]]
    named_transitions <- names(transition_triple)
    # The transitions arrive in a list which may be ordered but without names.
    if (!is.null(named_transitions)) {
      for (internal_name in names(internal_transitions)) {
        internal_transitions[[internal_name]][[transition_name]] <-
          transition_triple[[internal_name]]
      }
    } else {
      for (trans_idx in 1:length(internal_transitions)) {
        internal_transitions[[internal_name]][[trans_idx]] <-
          transition_triple[[trans_idx]]
      }
    }
  }
  # Set all the functions to execute within the environment.
  if (!is.null(variables)) {
    for (exec_trans in names(internal_transitions)) {
      for (trans_type in names(transitions)) {
        environment(
          internal_transitions[[exec_trans]][[trans_type]]) <- variables
      }
    }
  }

  time_names <- c("when", names(transitions))
  time_columns <- matrix(
    Inf, nrow = nrow(individuals), ncol = length(time_names))
  individuals[, time_names] <- data.table::as.data.table(time_columns)
  # Data.table will sort by firing time very quickly if we set this key.
  data.table::setkey(individuals, when)

  check_continuous_setup(individuals, internal_transitions)

  list(
    state = individuals,
    transitions = internal_transitions,
    variables = variables,
    observer = observer,
    time = 0,
    trajectory = vector(mode = "list", length = 100L),
    trajectory_cnt = 0L
    )
}


#' Given a simulation object, calculate enabled transitions.
#'
#' @param simulation This object is created by
#'     \code{\link{continuous_simulation}}
#' @export
init_continuous <- function(simulation) {
  transitions <- simulation$transitions
  # The current time and next firing times are part of the next state of the
  # system. If a firing time is Inf, that means it isn't scheduled.
  simulation$state <- initialize_times(
    simulation$state, transitions, simulation$time)

  # set up the observer
  if (is.null(simulation$observer)) {
    simulation$observer <- observe_continuous
  } else {
    if (!is.function(simulation$observer)) {
      stop(paste(c("if 'observer' slot in simulation is not NULL,",
                 "please provide it a function")))
    }
  }

  # return the simulation object
  simulation
}


# Stop when the next transition would exceed the time duration.
next_step_over_time <- function(duration) {
  function(state, step_cnt, current_time, next_time) {
    next_time >= duration
  }
}


#' Observe each transition.
#'
#' The memory use of this method could be improved. We could construct a
#' data.table into which to store the trajectory, so that it overwrites
#' lines in the data.table. By returning lists, we're churning memory.
#'
#' You may want to augment this to record the id of the individual.
#'
#' @param transition_name The string name of the transition.
#' @param former_state a list describing the individual's state
#'     before the transition
#' @param new_state a list describing the individual's state
#'     after the transition
#' @param time The time at which this transition fires.
#' @return a list with the name and time.
#' @export
observe_continuous <- function(transition_name, former_state, new_state, time) {
  list(name = transition_name, time = time)
}


#' Run the simulation for a certain time.
#'
#' @param simulation This object is created by
#'     \code{\link{continuous_simulation}}
#' @param duration How long to run
#' @return The trajectory of the run.
#' @export
run_continuous <- function(simulation, duration) {
  stopifnot(is.finite(duration))
  step_idx <- 0L
  current_time <- simulation$time
  stop_time <- current_time + duration
  stop_condition <- next_step_over_time(stop_time)
  observer <- simulation$observer
  individuals <- simulation$state
  if ("trajectory" %in% names(simulation)) {
    trajectory <- simulation$trajectory
  } else {
    trajectory <- vector(mode = "list", length = 256L)
  }
  # Use the count of trajectory entries in case someone calls this without
  # clearing the trajectory. This lets us tack onto the end of the trajectory.
  if ("trajectory_cnt" %in% names(simulation)) {
    trajectory_cnt <- simulation$trajectory_cnt
  } else {
    trajectory_cnt <- 0
  }
  while (TRUE) {
    soonest <- order(individuals$when)[1]
    individual <- as.list(individuals[soonest, ])
    should_end <- stop_condition(
      individuals, step_idx, current_time, individual$when)
    if (is.infinite(individual$when) || should_end) {
      break
    }
    new_state <- update_individual(individual, simulation$transitions, observer)
    individuals[soonest, ] <- new_state$individual
    current_time <- new_state$curtime
    step_idx <- step_idx + 1L
    trajectory_cnt <- trajectory_cnt + 1L
    if (trajectory_cnt > length(trajectory)) {
      trajectory <- c(trajectory, replicate(n = trajectory_cnt, expr = {NULL}))
    }
    trajectory[[trajectory_cnt]] <- new_state$entry
  }
  simulation$state <- individuals
  simulation$trajectory <- trajectory
  simulation$trajectory_cnt <- trajectory_cnt
  simulation$time <- stop_time
  simulation
}
