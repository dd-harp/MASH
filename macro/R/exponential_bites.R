#' Gives a constant rate of bites in a patch.
#'
#' @param parameters containing \code{people_cnt}, \code{bite_rate},
#'     \code{duration}.
#' @return A bite module.
#' @export
exponential_bite_module <- function(parameters) {
  params <- c("people_cnt", "bite_rate", "duration_days")
  stopifnot(names(parameters) %in% params)
  stopifnot(params %in% names(parameters))

  structure(list(parameters = parameters, bites = NULL, step_cnt = 0), class = "exponential_bite")
}


#' One time step of an exponential bite module.
#'
#' @param simulation the simulation object.
#' @return a modified simulation object
#' @export
mash_step.exponential_bite <- function(
    simulation, step_id, location_path, health_path) {
  simulation$bites <- with(simulation$parameters, {
    current_time <- duration_days * simulation$step_cnt
    forced_si_create_bites(people_cnt, bite_rate, current_time, duration_days)
    }
  )
  simulation$step_cnt <- simulation$step_cnt + 1
  simulation
}


#' Get the trajectory from a simulation time step.
#'
#' @param simulation an exponential bites simulation object.
#' @return bites a list with an entry for each person containing a vector
#'     of bite times.
#' @export
infects_human_path.exponential_bite <- function(simulation) {
  simulation$bites
}
