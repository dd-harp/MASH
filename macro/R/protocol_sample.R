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
