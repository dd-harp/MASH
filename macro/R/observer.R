#' Observe the change in location for every person.
#' @export
observe_location <- function(observer, location_path, step) {
  UseMethod("observe_location", observer)
}


#' Observe the bloodmeal bites to humans.
#' @export
observe_bloodmeal_human <- function(observer, bloodmeal_path, step) {
  UseMethod("observe_bloodmeal_human", observer)
}


#' Observe the bloodmeal bites to mosquitoes.
#' @export
observe_bloodmeal_mosquito <- function(observer, bloodmeal_path, step) {
  UseMethod("observe_bloodmeal_mosquito", observer)
}


#' Observe the health state for every person.
#' @export
observe_health <- function(observer, health_path, step) {
  UseMethod("observe_health", observer)
}

#' Observe the mosquito state.
#' @export
observe_mosquito <- function(observer, mosquito_path, step) {
  UseMethod("observe_mosquito", observer)
}

#' Called at beginning of the time step for any saving of observations.
#' @export
observe_begin_step <- function(observer, step) {
  UseMethod("observe_begin_step", observer)
}

#' Called at end of the time step for any saving of observations.
#' @export
observe_end_step <- function(observer, step) {
  UseMethod("observe_end_step", observer)
}


#' Observe every interaction among modules.
#'
#' @export
complete_observer <- function() {
  l <- list(
    location = list(),
    bloodmeal_human = list(),
    bloodmeal_mosquito = list(),
    health = list(),
    mosquito = list()
  )
  class(l) <- "complete_observer"
  l
}

#' Observe the change in location for every person.
#' @export
observe_location.complete_observer <- function(observer, location_path, step) {
  observer$location[[sprintf("%d", step)]] <- location_path
  observer
}


#' Observe the bloodmeal bites to humans.
#' @export
observe_bloodmeal_human.complete_observer <- function(observer, bloodmeal_human, step) {
  observer$bloodmeal_human[[sprintf("%d", step)]] <- bloodmeal_human
  observer
}


#' Observe the bloodmeal bites to mosquitoes.
#' @export
observe_bloodmeal_mosquito.complete_observer <- function(observer, bloodmeal_mosquito, step) {
  observer$bloodmeal_mosquito[[sprintf("%d", step)]] <- bloodmeal_mosquito
  observer
}


#' Observe the health state for every person.
#' @export
observe_health.complete_observer <- function(observer, health_path, step) {
  observer$health[[sprintf("%d", step)]] <- health_path
  observer
}


#' Observe the mosquito state.
#' @export
observe_mosquito.complete_observer <- function(observer, mosquito_path, step) {
  observer$mosquito[[sprintf("%d", step)]] <- mosquito_path
  observer
}

#' Called at beginning of the time step for any saving of observations.
#' @export
observe_begin_step.complete_observer <- function(observer, step) {
  observer
}

#' Called at end of the time step for any saving of observations.
#' @export
observe_end_step.complete_observer <- function(observer, step) {
  observer
}


#' Observer that stores no data
#'
#' @export
nothing_observer <- function() {
  l <- list()
  class(l) <- "nothing_observer"
  l
}

#' Observe the change in location for every person.
#' @export
observe_location.nothing_observer <- function(observer, location_path, step) {
  observer
}


#' Observe the bloodmeal bites to humans.
#' @export
observe_bloodmeal_human.nothing_observer <- function(observer, bloodmeal_human, step) {
  observer
}


#' Observe the bloodmeal bites to mosquitoes.
#' @export
observe_bloodmeal_mosquito.nothing_observer <- function(observer, bloodmeal_mosquito, step) {
  observer
}


#' Observe the health state for every person.
#' @export
observe_health.nothing_observer <- function(observer, health_path, step) {
  observer
}

#' Observe the mosquito state.
#' @export
observe_mosquito.nothing_observer <- function(observer, mosquito_path, step) {
  observer
}

#' Called at beginning of the time step for any saving of observations.
#' @export
observe_begin_step.nothing_observer <- function(observer, step) {
  observer
}

#' Called at end of the time step for any saving of observations.
#' @export
observe_end_step.nothing_observer <- function(observer, step) {
  observer
}
