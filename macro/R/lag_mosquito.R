# A trivial version of mosquito populations where infectiousness is lagged from bites.


#' Constant mosquito count with infections from past bites.
#'
#' This assumes the number of mosquitos in a place is constant.
#' The number of infectious mosquitoes is the number of incoming infectious bites
#' from `lag_days` ago, times the `decline_rate` parameter.
#'
#' On initialization, it uses a given start number, `initial_prevalence`, until the simulation has
#' progressed `lag_days`.
#'
#' @param parameters containing \code{people_cnt}, \code{bite_rate}, \code{duration}.
#' @return A bite module.
#' @export
lag_mosquito_module <- function(parameters) {
  params <- c("mosquito_cnt", "decline_rate", "lag_days", "initial_prevalence")
  mosy_obj <- list(parameters = params, past_bites = NULL, step_cnt = 0, duration = 10)
  structure(mosy_obj, class = "lag_mosquito")
}


#' One time step of an exponential bite module.
#'
#' @param simulation the simulation object.
#' @param bloodmeal_path A list of bites that are infectious to mosquitoes and times.
#'     These are the bites received during the last time step, not this one.
#' @return a modified simulation object
#' @export
mash_step.lag_mosquito <- function(simulation, bloodmeal_path) {
  # The path should be a list of biting times.
  stopifnot(is.numeric(bloodmeal_path))
  simulation$step_cnt <- simulation$step_cnt + 1
  if (is.null(simulation$past_bites)) {
    simulation$past_bites <- bloodmeal_path
  } else {
    # Reduce past bites if they go back too far.
    earliest_relevant <- simulation$step_cnt * simulation$duration - simulation$parameters$lag_days
    past_bites <- simulation$past_bites[simulation$past_bites > earliest_relevant]
    simulation$past_bites = c(past_bites, bloodmeal_path)
  }
  simulation
}


#' Get the trajectory from a simulation time step.
#'
#' @param simulation an exponential bites simulation object.
#' @return bites a list with an entry for each person containing a vector of bite times.
#' @export
mosquito_path.lag_mosquito <- function(simulation) {
  with(simulation, {
    early_past <- (step_cnt - 1) * duration - parameters$lag_days
    late_past <- step_cnt * duration - parameters$lag_days
    relevant <- bites[(bites >= early_past) & (bites < late_past)]
    culled <- relevant[dbinom(length(relevant), 1, decline_rate)]
    list(mosquito_cnt = parameters$mosquito_cnt, infectious = culled)
  })
}
