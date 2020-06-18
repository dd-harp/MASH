#' Observe every interaction among modules.
#'
#' @export
complete_observer <- function() {
  l <- list()
  class(l) <- "complete_observer"
  l
}


#' Observe the change in location for every person.
#' @export
observe_location <- function(observer, location_path, step) {

}

#' Observe the bloodmeal.
#' @export
observe_bloodmeal <- function(observer, bloodmeal_path, step) {

}

#' Observe the health state for every person.
#' @export
observe_health <- function(observer, health_path, step) {

}

#' Observe the mosquito state.
#' @export
observe_mosquito <- function(observer, mosquito_path, step) {

}

#' Called at beginning of the time step for any saving of observations.
#' @export
observe_begin_step <- function(observer, step) {

}

#' Called at end of the time step for any saving of observations.
#' @export
observe_end_step <- function(observer, step) {

}
