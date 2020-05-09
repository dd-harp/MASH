# --------------------------------------------------------------------------------
#
#   Flux model
#   May 2020
#
# --------------------------------------------------------------------------------

#' Movement: Make 'Flux' Transitions
#'
#' This function makes the set of transitions for the 'flux' model of movement;
#' the simple trip model is the simplest Eulerian movement model, meaning that it
#' individuals are indistinguishable. Because of this, for \eqn{N} patches, the state
#' is a vector of length \eqn{N} where each element gives the number of individuals there.
#'
#' @return a list of transitions
#'
#' @export
flux_transitions <- function() {

  transitions <- list()

  transitions$move <- list(
    # this is always enabled
    is_enabled = function(state, time) {
      return(TRUE)
    },
    when = function(state, time) {
      rexp(n = 1, rate = sum(rate_matrix[state$loc, ]))
    },
    fire = function(state, time) {
      dest <- sample.int(n = npatch, size = 1, prob = rate_matrix[state$loc, ])
      within(state, {
        loc <- dest
      })
    }
  )

  return(transitions)
}

#' Movement: Simple 'Flux' Observer
#'
#' The memory use of this method could be improved. We could construct a
#' data.table into which to store the trajectory, so that it overwrites
#' lines in the data.table. By returning lists, we're churning memory.
#'
#' @param transition_name The string name of the transition.
#' @param former_state a list describing the individual's state before the transition
#' @param new_state a list describing the individual's state after the transition
#' @param curtime The time at which this transition fires.
#' @return a list with the name and time.
#' @export
flux_observer <- function(transition_name, former_state, new_state, curtime) {
  list(
    name = transition_name,
    curtime = curtime,
    id = former_state[["who"]],
    prev_location = former_state[["loc"]],
    curr_location = new_state[["loc"]]
  )
}


#' Creates a MASH module that runs Flux
#'
#' This module simulates movement according to a 'flux' model, where everybody
#' is indistinguishable and moves to a new patch based on where they are now.
#'
#'  The input \code{parameters} must have the following *named* members:
#'    * \code{rate_matrix}: a matrix whose elements \eqn{\{f_{ij}\}} give the rate of movement
#'        between those two patches
#'    * \code{npatch}: number of patches/places
#'    * \code{location}: vector of everybody's current location
#'
#' @param parameters A list or environment
#'
#' @return A simulation object, which is a list.
#' @export
flux_module <- function(parameters) {

  expected_parameters <- c("rate_matrix","npatch","location")
  stopifnot(all(names(parameters) %in% expected_parameters))
  stopifnot(all(expected_parameters %in% names(parameters)))

  # parameter checking
  stopifnot(all(diag(parameters$rate_matrix) == 0))
  stopifnot(all(rowSums(parameters$rate_matrix) > 0))
  stopifnot(nrow(parameters$rate_matrix) == parameters$npatch &
    ncol(parameters$rate_matrix) == parameters$npatch
  )

  stopifnot(length(parameters$location) >= 1)

  # make the module structure
  transitions <- flux_transitions()

  people_cnt <- length(parameters$location)
  population <- data.table::data.table(
    who = 1:people_cnt,
    loc = parameters$location
  )

  simulation <- continuous_simulation(
    individuals = population,
    transitions = transitions,
    observer = flux_observer,
    variables = parameters
  )

  initialized_simulation <- init_continuous(simulation)
  class(initialized_simulation) <- "flux"

  return(initialized_simulation)
}


#' Takes one time step of the Flux model
#'
#' @param simulation A flux model (most likely made via \code{\link[macro]{flux_module}})
#' @param duration_days how long to step the simulation for
#' @param health_path The history of health states for each person.
#'
#' @export
mash_step.flux <- function(simulation, duration_days, health_path) {
  stopifnot(is.finite(duration_days))

  # clear trajectory before starting
  simulation$trajectory <- NULL
  simulation$trajectory_cnt <- NULL

  # run sim
  simulation <- run_continuous(simulation, duration_days)

  return(simulation)
}

#' Return trajectory by location for Flux model
#'
#' @param simulation A flux model (most likely made via \code{\link[macro]{flux_module}})
#' @return a list of \code{data.frame}, one for each patch giving the times anyone arrived at or left that patch
#' @export
location_path.flux <- function(simulation) {

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

    location_path[[event$prev_location]][which(is.nan(location_path[[event$prev_location]]$time))[1], ] <-
      c(arrive = NaN, leave = event$id, time = event$curtime)
    location_path[[event$prev_location]][which(is.nan(location_path[[event$curr_location]]$time))[1], ] <-
      c(arrive = event$id, leave = NaN, time = event$curtime)

  }

  # trim excess
  for(i in 1:npatch) {
    location_path[[i]] <- location_path[[i]][which(!is.nan(location_path[[i]]$time)), ]
  }

  return(location_path)
}


#' Return trajectory by person for Flux model
#'
#' @param simulation A flux model (most likely made via \code{\link[macro]{flux_module}})
#' @return a list of \code{data.frame}, one for person giving the time and new location when they moved
#' @export
person_path.flux <- function(simulation) {

  # the return object
  npeople <- nrow(simulation$state)
  nevent <- simulation$trajectory_cnt

  person_path <- replicate(n = npeople, expr = {
    data.frame(
      location = rep(NaN, nevent),
      time = rep(NaN, nevent)
    )
  }, simplify = FALSE)

  # process the events
  for(i in 1:nevent){
    if (is.null(simulation$trajectory[[i]])) {
      break
    }

    event <- simulation$trajectory[[i]]

    person_path[[event$id]][which(is.nan(person_path[[event$id]]$time)), ] <-
      c(location = event$curr_location, time = event$curtime)


  }

  # trim excess
  for(i in 1:npeople) {
    person_path[[i]] <- person_path[[i]][which(!is.nan(person_path[[i]]$time)), ]
  }


  return(simulation)
}
