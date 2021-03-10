# --------------------------------------------------------------------------------
#
#   Simple Trip model
#   April 2020
#
# --------------------------------------------------------------------------------

#' Movement: Make 'Simple Trip' Transitions
#'
#' This function makes the set of transitions for the 'simple trip' model of movement;
#' the simple trip model is the simplest Lagrangian (non-Eulerian) movement model, meaning that it
#' is not a flux type model of movement. Each person has a home, so for \eqn{N}
#' patches the state space is \eqn{N^{2}} as we track what patches residents of patch \eqn{i}
#' are at.
#'
#' @return a list of transitions
#'
#' @export
simple_trip_transitions <- function() {

  transitions <- list()

  transitions$take_trip <- list(
    is_enabled = function(state, time) {
      with(state, {
        return(home == current)
      })
    },
    when = function(state, time) {
      rexp(n = 1, rate = trip_rate[state$home])
    },
    fire = function(state, time) {
      dest <- sample.int(n = npatch, size = 1, prob = trip_dest[state$home, ])
      within(state, {
        current <- dest
      })
    }
  )

  transitions$return_home <- list(
    is_enabled = function(state, time) {
      with(state, {
        return(home != current)
      })
    },
    when = function(state, time) {
      rexp(n = 1, rate = return_home_rate[state$home, state$current])
    },
    fire = function(state, time) {
      within(state, {
        current <- home
      })
    }
  )

  return(transitions)
}

#' Movement: Simple 'Simple Trip' Observer
#'
#' The memory use of this method could be improved. We could construct a
#' data.table into which to store the trajectory, so that it overwrites
#' lines in the data.table. By returning lists, we're churning memory.
#'
#'
#' @param transition_name The string name of the transition.
#' @param former_state a list describing the individual's state before the transition
#' @param new_state a list describing the individual's state after the transition
#' @param curtime The time at which this transition fires.
#' @return a list with the name and time.
#' @export
simple_trip_observer <- function(transition_name, former_state, new_state, curtime) {
  list(
    name = transition_name,
    curtime = curtime,
    id = former_state[["who"]],
    prev_location = former_state[["current"]],
    curr_location = new_state[["current"]]
  )
}


#' Creates a MASH module that runs Simple Trip
#'
#' This module simulates movement according to a 'simple trip' model, where everybody
#' has a home which they leave to go on trips according to some rate. When they arrive at
#' a place they stay there for some time before returning home, upon which they schedule the
#' next trip.
#'
#'  The input \code{parameters} must have the following *named* members:
#'    * \code{trip_rate}: the rate at which each person takes a trip away from home
#'    * \code{trip_dest}: the stochastic matrix telling us where each person (row) goes
#'     (col) when they leave home
#'    * \code{return_home_rate}: the rate at which each person (row) goes home from their current location (col)
#'    * \code{npatch}: number of patches/places
#'    * \code{home}: vector of everybody's home
#'    * \code{current}: vector of everybody's current location
#'
#' @param parameters A list or environment. If you make an environment with
#'     `as.environment` then you may get an error "could not find function".
#'     That's because the environment inherits from the empty environment.
#'     Instead, use `list2env` to create the environment and it will, by
#'     default, inherit from the parent frame.
#'
#' @return A simulation object, which is a list.
#' @export
simple_trip_module <- function(parameters) {

  expected_parameters <- c(
    "trip_rate", "trip_dest", "return_home_rate", "npatch", "home",
    "current", "duration_days")
  stopifnot(all(names(parameters) %in% expected_parameters))
  stopifnot(all(expected_parameters %in% names(parameters)))

  # parameter checking
  stopifnot(all(diag(parameters$trip_dest) == 0))
  stopifnot(all(abs(rowSums(parameters$trip_dest) - 1) < 1e-15))
  stopifnot(nrow(parameters$trip_dest)==parameters$npatch & ncol(parameters$trip_dest)==parameters$npatch)

  stopifnot(nrow(parameters$return_home_rate)==parameters$npatch & ncol(parameters$return_home_rate)==parameters$npatch)
  stopifnot(all(parameters$return_home_rate >= 0))
  stopifnot(all(diag(parameters$return_home_rate) == 0))

  stopifnot(length(parameters$home) == length(parameters$current))

  # make the module structure
  transitions <- simple_trip_transitions()

  people_cnt <- length(parameters$home)
  population <- data.table::data.table(
    who = 1:people_cnt,
    home = parameters$home,
    current = parameters$current
  )

  simulation <- continuous_simulation(
    individuals = population,
    transitions = transitions,
    observer = simple_trip_observer,
    variables = parameters
  )

  initialized_simulation <- init_continuous(simulation)
  class(initialized_simulation) <- "simple_trip"

  return(initialized_simulation)
}


#' Takes one time step of the Simple Trip model
#'
#' @param simulation A simple trip model (most likely made via \code{\link[macro]{simple_trip_module}})
#' @param duration_days how long to step the simulation for
#' @param health_path The history of health states for each person.
#'
#' @export
mash_step.simple_trip <- function(simulation, step_id, health_path) {
  stopifnot(is.finite(simulation$variables$duration_days))

  # clear trajectory before starting
  simulation$trajectory <- NULL
  simulation$trajectory_cnt <- NULL

  # run sim
  simulation <- run_continuous(simulation, simulation$variables$duration_days)

  return(simulation)
}


#' Return trajectory by location for Simple Trip model
#'
#' @param simulation A simple trip model (most likely made via \code{\link[macro]{simple_trip_module}})
#' @return a list of \code{data.frame}, one for each patch giving the times anyone arrived at or left that patch
#' @export
location_path.simple_trip <- function(simulation) {

  # the return structure
  nevent <- simulation$trajectory_cnt
  npatch <- simulation$variables$npatch

  location_path <- replicate(n = npatch, expr = {
    data.frame(
      arrive = rep(NaN, nevent),
      leave = rep(NaN, nevent),
      time = rep(NaN, nevent)
    )
  }, simplify = FALSE)

  # process the events
  for(i in 1:nevent){
    if (is.null(simulation$trajectory[[i]])) {
      break
    }

    event <- simulation$trajectory[[i]]

    location_path[[event$prev_location]][which(is.nan(location_path[[event$prev_location]]$time))[1], ] <- c(arrive = NaN, leave = event$id, time = event$curtime)
    location_path[[event$prev_location]][which(is.nan(location_path[[event$curr_location]]$time))[1], ] <- c(arrive = event$id, leave = NaN, time = event$curtime)

  }

  # trim excess
  for(i in 1:npatch) {
    location_path[[i]] <- location_path[[i]][which(!is.nan(location_path[[i]]$time)), ]
  }

  return(location_path)
}


#' Return trajectory by person for Simple Trip model
#'
#' @param simulation A simple trip model (most likely made via \code{\link[macro]{simple_trip_module}})
#' @return a list of \code{data.frame}, one for person giving the time and new location when they moved
#' @export
person_path.simple_trip <- function(simulation) {

  # the return object
  npeople <- nrow(simulation$state)
  nevent <- simulation$trajectory_cnt

  # The trajectory can accommodate more events, so truncate it.
  trajectory <- data.table::rbindlist(simulation$trajectory[1:nevent])
  # The first move tells us where they started.
  first_moves <- trajectory[
    trajectory[, .I[curtime == min(curtime)], by = id]$V1]
  first_location <- first_moves[, .(id, prev_location)]
  data.table::setnames(first_location, c("id"), c("ID"))
  initial_time <- simulation$time - simulation$variables$duration_days
  # Build initial state from the final state and the first jump.
  final_state <- data.table::data.table(
    ID = simulation$state$who,
    Location = simulation$state$current,
    Time = initial_time
  )
  initial_state <- merge(
    final_state,
    first_location,
    by = "ID",
    all.x = TRUE
  )
  initial_state[!is.na(initial_state$prev_location), .(Location = prev_location)]
  initial_state[, prev_location := NULL]

  onlycols <- trajectory[, .(id, curr_location, curtime)]
  data.table::setnames(
    onlycols, c("id", "curr_location", "curtime"), c("ID", "Location", "Time"))

  return(rbind(initial_state, onlycols))
}


#' Movement: Simple Trip Trajectory to State Probabilities
#'
#' This is an internal function used to verify that the 'simple trip'
#' model is functioning correctly. See the simple trip vignette
#' for more details.
#'
#' @param trajectory the output of \code{\link[macro]{run_continuous}}
#' @param init_state the initial state (S1,S2,S3,S4)
#' @return a vector of time-averaged state occupancy probabilities
simple_trip_stateoutput <- function(trajectory, init_state) {

  stopifnot(init_state %in% c("S1","S2","S3","S4"))

  state_trans <- function(current, id, loc) {
    if(current == "S1"){
      if(id==1 & loc==2){
        return("S2")
      } else if(id==2 & loc==2){
        return("S3")
      } else {
        cat("illegal value\n")
        browser()
      }
    } else if(current == "S2"){
      if(id==1 & loc==1){
        return("S1")
      } else if(id==2 & loc==2){
        return("S4")
      } else {
        cat("illegal value\n")
        browser()
      }
    } else if(current == "S3"){
      if(id==1 & loc==2){
        return("S4")
      } else if(id==2 & loc==1){
        return("S1")
      } else {
        cat("illegal value\n")
        browser()
      }
    } else if(current == "S4"){
      if(id==1 & loc==1){
        return("S3")
      } else if(id==2 & loc==1){
        return("S2")
      } else {
        cat("illegal value\n")
        browser()
      }
    } else {
      cat("illegal value\n")
      browser()
    }
  }

  state_occupancy <- rep(0,4)
  state_occupancy <- setNames(state_occupancy,paste0("S",1:4))
  curr_state <- init_state

  for(i in 1:nrow(trajectory)){
    if(i==1){
      state_occupancy[curr_state] <- state_occupancy[curr_state] + trajectory[i,"curtime"]
      curr_state <- state_trans(curr_state,trajectory[i,"id"],trajectory[i,"curr_location"])
    } else {
      state_occupancy[curr_state] <- state_occupancy[curr_state] + (trajectory[i,"curtime"] - trajectory[i-1,"curtime"])
      curr_state <- state_trans(curr_state,trajectory[i,"id"],trajectory[i,"curr_location"])
    }
  }

  state_occupancy <- state_occupancy / tail(trajectory,1)[["curtime"]]

  return(state_occupancy)
}
