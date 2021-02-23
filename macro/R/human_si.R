human_si_step_days <- function(compartments, bites, start_time, parameters) {
  daily_events <- vector(mode = "list", length = parameters$duration_days)
  for (day_idx in 1:parameters$duration_days) {
    recover <- rbinom(parameters$people_cnt, 1, parameters$recovery_rate)
    infect <- integer(parameters$people_cnt)

    bites_can_be_missing_for_future <- is.null(bites)
    if (!bites_can_be_missing_for_future) {
      for (bidx in 1:parameters$people_cnt) {
        day_bites <- bites[bidx, times][[1]]
        if (length(day_bites) > 0) {
          today_bites <- which((start_time + day_idx - 1 <= day_bites) &
                                 (day_bites < start_time + day_idx))
          if (length(today_bites) > 0) {
            infect[bidx] <- 1
          }
        }
      }
    }

    infections <- which(compartments[, 1] & infect)
    recoveries <- which(compartments[, 2] & recover)

    id <- c(infections, recoveries)
    time <- rep(start_time + day_idx - 1 + runif(1),
                 length(infections) + length(recoveries))
    level <- c(rep(1, length(infections)), rep(0, length(recoveries)))
    daily_events[[as.character(day_idx)]] <- data.table::data.table(
      ID = id,
      Time = time,
      Level = level
    )
    compartments[infections, ] <- c(0, 1)
    compartments[recoveries, ] <- c(1, 0)
  }
  list(
    compartments = compartments,
    events = do.call(rbind, daily_events)
  )
}


#' Create human_si module.
#' @param parameters A list with "recovery_rate", "people_cnt",
#'     "duration_days", "initial_pfpr".
#' @export
human_si_module <- function(parameters) {
  expected_parameters <- c(
    "recovery_rate", "people_cnt", "duration_days", "initial_pfpr")
  stopifnot(all(names(parameters) %in% expected_parameters))
  stopifnot(all(expected_parameters %in% names(parameters)))
  # This is the definition of the state, aka compartments.
  # It is a matrix of people x (uninfected, infected), as 0 or 1.
  matrix_state <- matrix(0, nrow = parameters$people_cnt, ncol = 2)
  matrix_state[, 2] <- rbinom(parameters$people_cnt, 1, parameters$initial_pfpr)
  matrix_state[, 1] <- 1 - matrix_state[, 2]
  simulation <- list(
    parameters = parameters,
    state = matrix_state,
    initial_state = matrix_state,
    events = NULL,
    time = 0
  )
  step_result <- human_si_step_days(
    simulation$state,
    NULL,
    simulation$time,
    simulation$parameters
  )
  simulation$events <- step_result$events
  class(simulation) <- "human_si"
  simulation
}


#' One step of the human_si module.
#' @param simulation The module.
#' @param bites_dt a data.table of bites.
#' @export
mash_step.human_si <- function(simulation, bites_dt) {
  # The first step has correct information, so we save its state.
  initial_state <- simulation$state
  step_result <- human_si_step_days(
    simulation$state,
    bites_dt,
    simulation$time,
    simulation$parameters
    )
  simulation$state <- step_result$compartments
  simulation$time = simulation$time + simulation$parameters$duration_days
  # The second step has incorrect bites, so we don't save its state
  # but use its correct recovery events.
  step_result <- human_si_step_days(
    simulation$state,
    NULL,
    simulation$time,
    simulation$parameters
  )
  simulation$initial_state <- initial_state
  simulation$events <- step_result$events
  class(simulation) <- "human_si"
  simulation
}


#' Get events from the human_si module.
#' @param simulation The module.
#' @export
human_disease_path.human_si <- function(simulation) {
  parameters <- simulation$parameters
  # Needs one row per person.
  # Titles are ID, Start, Time1, Level1, Time2, Level2
  initial <- data.table::data.table(
    ID = 1:parameters$people_cnt,
    Time = 0,
    Level = simulation$initial_state[, 2]
  )
  events <- simulation$events[order(simulation$events$Time), .(ID, Time, Level)]
  rbind(initial, events)
}
