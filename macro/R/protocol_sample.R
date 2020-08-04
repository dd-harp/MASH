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
