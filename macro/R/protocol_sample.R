# Makes examples of data for passing among modules.
library(data.table)


#' Creates a dataset that health would send to bloodmeal.
#'
#' @param human_cnt The number of humans as an integer.
#' @param time_step The length of a time step as a numeric.
#'
#' You can use this to get a look at what the data would be.
#'
#' @export
sample_health_infection_status <- function(human_cnt = 10L, time_step = 10.0) {
  infected_fraction = 0.4
  recover_fraction <- 0.5
  infect_fraction <- 1.0 - recover_fraction
  start_infected <- rep(1.0, infected_fraction * human_cnt)
  recover_cnt <- as.integer(length(start_infected) * recover_fraction)
  infect_cnt <- recover_cnt
  titles <- paste0(c("Time", "Level"), rep(1:3, each = 2L))
  events <- data.table(
    ID = sample(1:human_cnt),
    Start = c(start_infected, rep(0.0, human_cnt - length(start_infected)))
  )
  events[, titles] <- 0.0  # So that column has type numeric.
  events[, titles] <- NA

  events[1:recover_cnt, "Time1"] <- runif(recover_cnt, 0, time_step)
  events[1:recover_cnt, "Level1"] <- numeric(recover_cnt)
  events[1, "Time2"] = runif(1, events[1, Time1], time_step)
  events[1, "Level2"] = 1.0

  events[(human_cnt - infect_cnt + 1):human_cnt, "Time1"] <- runif(infect_cnt, 0, time_step)
  events[(human_cnt - infect_cnt + 1):human_cnt, "Level1"] <- rep(1.0, infect_cnt)

  events[order(ID)]
}


#' Creates a sample movement dataset, that would be sent to the bloodmeal module.
#'
#' @param human_cnt The number of people as an integer.
#' @param place_cnt The number of possible locations as an integer.
#' @param time_step The length of a time step as a numeric.
#'
#' @export
sample_move_location <- function(human_cnt = 10L, place_cnt = 5L, time_step = 10.0) {
  human_cnt <- 10L
  place_cnt <- 3L
  time_step <- 10.0
  rate <- 0.2

  events <- data.table(
    ID = sample(1:human_cnt),
    Start = sample(1:place_cnt, human_cnt, replace = TRUE)
  )

  move_cnt <- human_cnt
  moving <- 1L:move_cnt
  location <- events$Start
  now <- rep(0.0, move_cnt)
  move_idx <- 1L

  while (move_cnt > 0) {
    when <- now[moving] + rexp(move_cnt, rate)
    in_time <- when < time_step
    moving <- moving[in_time]
    if (length(moving) > 0) {
      next_location <- unlist(lapply(moving, function(who) {
        sample(setdiff(1:place_cnt, location[who]), 1)
      }))
      time_col <- paste0("Time", move_idx)
      loc_col <- paste0("Location", move_idx)
      events[, time_col] <- 0.0
      events[, time_col] <- NA
      events[, loc_col] <- 1L
      events[, loc_col] <- NA
      events[moving, time_col] <- when[in_time]
      events[moving, loc_col] <- next_location
      now[moving] <- when[in_time]
      location[moving] <- next_location
    }
    move_cnt <- length(moving)
    move_idx <- move_idx + 1L
  }
  events
}


#' Create a sample dataset from mosquitoes to bloodmeal.
#'
#' @param bite_cnt Integer number of bites.
#' @param place_cnt Integer number of locations to do the biting.
#' @param time_step Duration of time step within which to bite.
#'
#' @export
sample_mosquito_half_bites <- function(bite_cnt = 10L, place_cnt = 3L, time_step = 10.0) {
  infected_rate <- 0.3

  events <- data.table(
    Location = sample(1:place_cnt, bite_cnt, replace = TRUE),
    Bite = rbinom(bite_cnt, 1, infected_rate) * runif(bite_cnt, .3, 1.0),
    Time = runif(bite_cnt, 0, time_step)
  )
  events[order(Location, Time)]

}


#' Create a sample dataset from mosquitoes to bloodmeal.
#'
#' @param place_cnt Integer number of locations to do the biting.
#' @param time_step Duration of time step within which to bite.
#'
#' @export
sample_mosquito_kappa <- function(place_cnt = 3L, time_step = 10.0) {
  infected_rate <- runif(place_cnt, 0, 100)
  step_cnt <- as.integer(time_step)
  bites <- sapply(infected_rate, function(x) {rpois(step_cnt, x)})
  events <- data.table(
    Location = rep(1:place_cnt, step_cnt),
    Bites = as.numeric(t(bites)),
    Time = rep(1:step_cnt - 1, each = place_cnt)
  )
  events
}



#' Sample dataset output from mosquito to bloodmeal using (M, Y, Z).
#'
#' @param place_cnt Integer number of locations to do the biting.
#' @param time_step Duration of time step within which to bite.
#'
#' @export
sample_mosquito_myz <- function(place_cnt = 3L, time_step = 10.0) {
  step_cnt <- as.integer(time_step)
  adults_order.l <- sample(0:4 * 500, place_cnt)
  adults.l <- round(rpois(place_cnt * step_cnt, rep(adults_order.l, step_cnt)))
  infected.l <- runif(place_cnt, 0, .40)
  biting_rate <- rep(runif(place_cnt, 0.3, 0.5), step_cnt)
  events <- data.table(
    Location = rep(1:place_cnt, step_cnt),
    Time = rep(1:step_cnt - 1, each = place_cnt),
    a = biting_rate,
    M = adults.l,
    infected_rate = rep(infected.l, place_cnt)
  )
  SYZ <- t(vapply(
    1:nrow(events),
    function(row_idx) {
      infr <- events[row_idx, "infected_rate"]
      rmultinom(1, events[row_idx]$M, c(1 - 2 * infr, infr, infr))
    },
    numeric(3)
  ))
  events[, Y := SYZ[, 2]]
  events[, Z := SYZ[, 3]]
  events[, c("Location", "Time", "a", "M", "Y", "Z")]
}


#' Create a sample dataset from bloodmeal to mosquitoes.
#'
#' @param place_cnt Integer number of locations to do the biting.
#' @param time_step Duration of time step within which to bite.
#'
#' @export
sample_mosquito_eip <- function(place_cnt = 3L, time_step = 10.0) {
  infected_rate <- runif(place_cnt, 0, 100)
  step_cnt <- as.integer(time_step)
  bites <- sapply(infected_rate, function(x) {rpois(step_cnt, x)})
  events <- data.table(
    Location = rep(1:3, step_cnt),
    EIP = as.numeric(t(bites)),
    Time = rep(1:step_cnt - 1, each = place_cnt)
  )
  events
}


one_human_at_location <- function(id) {
  time_step = 10
  start_infection <- rbinom(1, 1, 0.5)
  change_infection_time <- rexp(1, 1 / time_step)
  infection_dt <- data.table(
    Time = c(-1, change_infection_time),
    Level = c(start_infection, 1 - start_infection),
    Event = c(2, 2)
  )

  move_period <- 5
  move_times <- cumsum(rexp(9, 1 / move_period))
  enter_leave <- rep(c(0, 1), 5)
  start_inside <- rbinom(1, 1, 0.5)
  if (start_inside != 0) {
    enter_leave <- 1 - 2 * enter_leave
  } else {
    enter_leave <- -1 + 2 * enter_leave
  }
  move_dt <- data.table(
    Time = c(0, move_times),
    Level = 0,
    Event = enter_leave
  )

  unordered_dt <- rbind(infection_dt, move_dt)
  all_dt <- unordered_dt[order(Time),]

  level <- 0
  for (idx in 1:nrow(all_dt)) {
    if (all_dt[idx, Event] == 2) {
      level <- all_dt[idx, Level]
    } else {
      all_dt[idx]$Level <- level
    }
  }
  all_dt[, ID := id]
  all_dt
}


#' Sample of human activity at a location, within the bloodmeal.
#'
#' @param human_cnt
#' @param rate
#' @param time_step Defaults to 10.
#' @return A data.table with the columns `ID`, `Level`, `Time`
#'     `Location`, and `Event`. The location will be 1 for all
#'     rows. The event is 1 for enter, -1 for leave and 2 for
#'     change in infection status. The level is 0 or 1.
sample_humans_at_location <- function(human_cnt = 10L, move_period = 20, time_step = 10) {
  pfpr <- 0.4
  start_infectious <- sample(1:human_cnt, round(pfpr * human_cnt))
  change_infection_time <- rexp(human_cnt, 1 / time_step)
  start_in_location <- sample(1:human_cnt, round(0.6 * human_cnt))
  ddt <- do.call(rbind,lapply(1:10, one_human_at_location))
  in_time <- ddt[Time >= 0 & Time < time_step,]
  in_order <- in_time[order(ID, Time),]
  in_order[, Location := 1]
  in_order[, .(ID, Level, Time, Location, Event)]
}
