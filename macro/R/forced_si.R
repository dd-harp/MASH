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
    is_enabled = function(state, time) with(state, disease == "S" && bites[length(bites)] > time),
    when = function(state, time) with(state, {
      next_bite <- which(bites > time)
      ifelse(length(next_bite) > 0L, bites[next_bite], Inf)
    }),
    fire = function(state, time) within(state, {disease = "I"})
  )
}


#' Recover an individual from infection
#'
#' @param rate The rate parameter for an exponential distribution
#' @return A list with an enabling function, firing time, and firing function.
#' @export
forced_si_recover <- function(rate) {
  list(
    is_enabled = function(state, time) with(state, disease == "I"),
    when = function(state, time) rexp(1, rate),
    fire = function(state, time) within(state, {disease = "S"})
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
    disease = factor(ifelse(rbinom(people_cnt, 1, 0.4), "I", "S"), levels = c("S", "I")),
    bites = lapply(1:people_cnt, function(x) numeric(0))
    )
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

