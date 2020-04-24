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
      state$current <- dest
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
      state$current <- state$home
    }
  )

  return(transitions)
}
