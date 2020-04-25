# --------------------------------------------------------------------------------
#
#   Simple Trip model
#   Sean L. Wu (slwu89@berkeley.edu)
#   April 2020
#
# --------------------------------------------------------------------------------

#' Movement: Make 'Simple Trip' Transitions
#'
#' This function makes the set of transitions for the 'simple trip' model of movement;
#' the simple trip model is the simplest non-Eulerian movement model, meaning that it
#' is not a flux type model of movement. Each person has a home, so for \eqn{N}
#' patches the state space is \eqn{N^{2}} as we track what patches residents of patch \eqn{i}
#' are at.
#'
#'
#' @return a list
#'
#' @export
simple_trip_transitions <- function(){

  transitions <- list()

  transitions$take_trip <- list(
    is_enabled = function(state,time){
      with(state,{
        return(home == current)
      })
    },
    when = function(state,time){
      rexp(n=1,rate=trip_rate[state$home])
    },
    fire = function(state,time){
      dest <- sample.int(n=npatch,size=1,prob=trip_dest[state$home,])
      within(state,{
        current <- dest
      })
    }
  )

  transitions$return_home <- list(
    is_enabled = function(state,time){
      with(state,{
        return(home != current)
      })
    },
    when = function(state,time){
      rexp(n=1,rate=return_home_rate[state$home,state$current])
    },
    fire = function(state,time){
      within(state,{
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
#' Currently this is identical to \code{\link[macro]{forced_si_observer}}
#'
#' You may want to augment this to record the id of the individual.
#'
#' @param transition_name The string name of the transition.
#' @param former_state a list describing the individual's state before the transition
#' @param new_state a list describing the individual's state after the transition
#' @param curtime The time at which this transition fires.
#' @return a list with the name and time.
#' @export
simple_trip_observer <- function(transition_name, former_state, new_state, curtime) {
  list(name = transition_name, curtime = curtime, id = former_state[["who"]], location = new_state[["current"]])
}
