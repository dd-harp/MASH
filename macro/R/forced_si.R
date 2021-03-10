library(data.table)

#' Infect an individual from a given set of mosquito bite times.
#'
#' We assume the individual state includes a list of times for
#' infectious bites. This reads that list in order to determine
#' when they next get bitten.
#'
#' @return A list with an enabling function, firing time, and firing function.
#' @export
forced_si_infect <- function() {
  list(
    is_enabled = function(state, curtime) with(
      state,
      {
        # The bites are a vector of biting times, but we assume they are packed into
        # a list because this is how they have to be stored in a dataframe.
        bite_vec <- bites[[1]]
        disease == "S" && (length(bite_vec) > 0) && (bite_vec[length(bite_vec)] > curtime)
      }
      ),
    when = function(state, curtime) with(state, {
      # The time of infection is given by a list of bites, so this
      # doesn't sample for times.
      bite_vec <- bites[[1]]
      next_bite <- which(bite_vec > curtime)
      ifelse(length(next_bite) > 0L, bite_vec[next_bite[1]], Inf)
    }),
    fire = function(state, curtime) within(state, {disease = "I"})
  )
}


#' Recover an individual from infection
#'
#' @param rate The rate parameter for an exponential distribution
#' @return A list with an enabling function, firing time, and firing function.
#' @export
forced_si_recover <- function(rate) {
  list(
    is_enabled = function(state, curtime) with(state, disease == "I"),
    when = function(state, curtime) rexp(1, rate),
    fire = function(state, curtime) within(state, {disease = "S"})
  )
}


#' Create a population of individuals for forced-SI
#'
#' @param people_cnt How many people
#' @param pfpr What percentage of them should start infected.
#' @return a data.table with columns disease=factor(S, I) and bites,
#'     which is an empty list.
#' @export
forced_si_population <- function(people_cnt, pfpr) {
  data.table::data.table(
    who = 1L:people_cnt,  # identify each person.
    disease = factor(ifelse(rbinom(people_cnt, 1, pfpr), "I", "S"),
      levels = c("S", "I")),
    bites = lapply(1:people_cnt, function(x) numeric(0))
    )
}


#' Observe each transition.
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
forced_si_observer <- function(transition_name, former_state, new_state, curtime) {
  list(name = transition_name, curtime = curtime, id = former_state[["who"]])
}


#' Generate bites for a population of a forced_si model.
#'
#' @param people_cnt How many people there are
#' @param bite_rate The number of bites per day.
#' @param current_time The start time for bite events
#' @param duration in days
#' @return a list of arrays of bite times
#' @examples
#' \dontrun{
#' pop <- forced_si_population(5L, 0.4)
#' pop[, "bites"] <- forced_si_create_bites(nrow(pop), 0.2, 0, 14)
#' }
#' @export
forced_si_create_bites <- function(people_cnt, bite_rate, current_time, duration) {
  lapply(1:people_cnt, function(x) {
    # Generate 3x too many bites so that they likely cover the duration.
    bites <- cumsum(rexp(round(bite_rate * duration * 3), bite_rate))
    current_time + bites[bites < duration]
  })
}


#' Creates a MASH module that runs forced SI.
#'
#' This module describes a human as being in an S state or an I state.
#' An external list of bites forces humans into the I state.
#'
#' @param parameters These are simulation parameters. They are
#'     \code{recovery_rate} for recovery, \code{people_cnt} for
#'     the number of individuals, \code{duration_days} for the number
#'     of days per time step. If there are too few or too many parameters,
#'     then this rejects the argument. We disallow extra parameters so that
#'     misnamed parameters don't go unnoticed.
#' @return A simulation object, which is a list.
#' @export
forced_si_module <- function(parameters) {
  expected_parameters <- c(
    "recovery_rate", "people_cnt", "duration_days", "initial_pfpr")
  stopifnot(all(names(parameters) %in% expected_parameters))
  stopifnot(all(expected_parameters %in% names(parameters)))
  stopifnot(is.finite(parameters$initial_pfpr))
  stopifnot(0 <= parameters$initial_pfpr)
  stopifnot(parameters$initial_pfpr < 1)

  transitions <- list(
    infect = forced_si_infect(),
    recover = forced_si_recover(parameters$recovery_rate)
  )
  people_cnt <- as.integer(parameters$people_cnt)
  people_state <- forced_si_population(people_cnt, parameters$initial_pfpr)
  stopifnot(colnames(people_state) == c("who", "disease", "bites"))

  simulation <- continuous_simulation(
    people_state,  # The `state` variable will be this plus time columns.
    transitions,
    forced_si_observer
  )
  simulation[["parameters"]] <- parameters
  initialized_simulation <- init_continuous(simulation)
  class(initialized_simulation) <- "forced_si"
  initialized_simulation
}


#' Takes one time step of the discrete time step.
#'
#' @param simulation A forced-SI model.
#' @param bites A datatable where each row has a human=ID
#'     and a bites=list(array of bite times).
#' @return a simulation object
#' @examples
#' \dontrun{
#' step_si_module(simulation,
#'                forced_si_create_bites(100, 1/20, current_time, 14))
#' }
#' @export
mash_step.forced_si <- function(simulation, step_id, bites) {
  # Save previous state so we know start what the recorded events are changing.
  simulation$previous_state <- simulation$state[, .(who, disease)]
  # We need to turn the bites into events within the stochastic system.
  bites <- bites[order(human), ]
  next_bite <- vapply(
    1:nrow(bites),
    function(r) {
      bite_vec <- bites[r, times]
      stopifnot(is.vector(bite_vec))
      if (length(bite_vec) > 0) {
        min(bite_vec)
      } else {
        Inf  # never is a valid value.
      }
    },
    numeric(1)
  )
  infections <- data.table::data.table(
    human = bites$human, bite_time = next_bite)
  data.table::setkey(infections, "human")
  data.table::setkey(simulation$state, "who")
  joined <- infections[simulation$state]
  joined[joined$disease == 'S' & !is.na(joined$bite_time), when := bite_time]
  joined[joined$disease == 'S' & !is.na(joined$bite_time), infect := bite_time]
  joined[, bite_time := NULL]
  setnames(joined, "human", "who")
  simulation$state <- joined
  run_continuous(simulation, simulation$parameters$duration_days)
}


#' Extract the trajectory from the simulation
#'
#' @param simulation a forced-SI model.
#' @return a list of trajectory entries. Each trajectory entry has the individual's ID,
#'     their initial state as S or I, and times at which they changed state.
#' @export
human_disease_path.forced_si <- function(simulation) {
  # Create a wide dataframe with a column for each time the state changes.
  health_dt <- data.table(
    ID = simulation$previous_state$who,
    Start = ifelse(simulation$previous_state$disease == "I", 1, 0)
    )
  # Add some NA columns so we don't have to expand while working.
  health_dt[, paste0(c("Time", "Level"), rep(1:4, each = 2))] <- 0
  health_dt[, paste0(c("Time", "Level"), rep(1:4, each = 2))] <- NA
  if (simulation$trajectory_cnt > 0) {
    trajectory <- simulation$trajectory[1:simulation$trajectory_cnt]
    for (tidx in 1:simulation$trajectory_cnt) {
      entry <- trajectory[tidx][[1]]
      who <- entry$id
      next_event <- (sum(!is.na(health_dt[who,])) - 2) %/% 2 + 1
      tstring <- paste0("Time", next_event)
      lstring <- paste0("Level", next_event)
      health_dt[who, (tstring) := entry$curtime]
      level <- ifelse(entry$name == "infect", 1, 0)
      health_dt[who, (lstring) := level]
    }
  }
  health_dt
}
