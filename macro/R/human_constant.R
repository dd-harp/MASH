#' Create human_constant module.
#' @param parameters A list with "recovery_rate", "people_cnt",
#'     "duration_days", "initial_pfpr".
#' @export
human_constant_module <- function(parameters) {
  expected_parameters <- c(
    "people_cnt", "duration_days", "initial_pfpr")
  stopifnot(all(expected_parameters %in% names(parameters)))
  # This is the definition of the state, aka compartments.
  # It is a matrix of people x (uninfected, infected), as 0 or 1.
  matrix_state <- matrix(0, nrow = parameters$people_cnt, ncol = 2)
  matrix_state[, 2] <- rbinom(parameters$people_cnt, 1, parameters$initial_pfpr)
  matrix_state[, 1] <- 1 - matrix_state[, 2]
  simulation <- list(
    parameters = parameters,
    state = matrix_state,
    events = NULL,
    time = 0
  )
  simulation$events <- data.table::data.table(
    ID = 1:parameters$people_cnt,
    Time = 0,
    Level = matrix_state[, 2]
  )
  class(simulation) <- "human_constant"
  simulation
}


#' One step of the human_constant module.
#' @param simulation The module.
#' @param bites_dt a data.table of bites.
#' This ignores its imput and updates the time.
#' @export
mash_step.human_constant <- function(simulation, step_id, bites_dt) {

  simulation$time = simulation$time + simulation$parameters$duration_days
  simulation$events[, `:=`(Time = simulation$time)]
  class(simulation) <- "human_constant"
  simulation
}


#' Get events from the human_si module.
#' @param simulation The module.
#' @export
human_disease_path.human_constant <- function(simulation) {
  simulation$events
}
