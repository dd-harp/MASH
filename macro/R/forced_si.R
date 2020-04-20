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
        bite_vec <- bites[[1]]
        disease == "S" && (length(bite_vec) > 0) && (bite_vec[length(bite_vec)] > curtime)
      }
      ),
    when = function(state, curtime) with(state, {
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
    disease = factor(ifelse(rbinom(people_cnt, 1, 0.4), "I", "S"), levels = c("S", "I")),
    bites = lapply(1:people_cnt, function(x) numeric(0))
    )
}


#' Observe each transition.
#'
#' The memory use of this method could be improved. We could construct a
#' data.table into which to store the trajectory, so that it overwrites
#' lines in the data.table. By returning lists, we're churning memory.
#'
#' You may want to augment this to record the id of the individual.
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
#' @example
#' pop <- forced_si_population(5L, 0.4)
#' pop[, "bites"] <- forced_si_create_bites(nrow(pop), 0.2, 14)
#'
#' @export
forced_si_create_bites <- function(people_cnt, bite_rate, current_time, duration) {
  lapply(1:people_cnt, function(x) {
    bites <- cumsum(rexp(round(bite_rate * duration * 3), bite_rate))
    current_time + bites[bites < duration]
  })
}

