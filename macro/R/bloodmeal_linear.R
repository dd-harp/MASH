library(data.table)

convert_to_source_destination <- function(location_dt) {
  location_linear <- location_dt[, .(ID = ID, Previous = NA, Location = Start, Time = 0.0)]
  step_cnt <- length(grep("Time", names(location_dt)))
  locs <- list(location_linear)
  previous_location <- location_linear[, Location]
  for (step_idx in 1:step_cnt) {
    location_name <- paste0("Location", step_idx)
    time_name <- paste0("Time", step_idx)
    dest <- location_dt[, ..location_name][[1]]
    when <- location_dt[, ..time_name][[1]]
    step_dt <- location_dt[
      ,
      .(ID = ID, Previous = previous_location, Location = dest, Time = when)
      ]
    previous_location <- step_dt$Location
    locs[[step_idx + 1]] <- step_dt
  }
  all_locs <- do.call(rbind, locs)
  all_locs[is.finite(Location)]
}


convert_to_enter_events <- function(location_changes_dt) {
  enter <- location_changes_dt[
    , .(ID = ID, Location = Location, Time = Time, Event = 1)]
  leave <- location_changes_dt[
    is.finite(Previous), .(ID = ID, Location = Previous, Time = Time, Event = -1)]
  events <- rbind(enter, leave)
  events[order(Location, Time)]
}


health_as_events <- function(health_dt) {
  h0 <- health_dt[, .(ID = ID, Level = Start, Time = 0.0)]
  step_cnt <- length(grep("Time", names(location_dt)))
  healths <- list(h0)
  for (step_idx in 1:step_cnt) {
    level_name <- paste0("Level", step_idx)
    time_name <- paste0("Time", step_idx)
    level <- health_dt[, ..level_name][[1]]
    when <- health_dt[, ..time_name][[1]]
    h <- health_dt[, .(ID = ID, Level = level, Time = when)]
    healths[[step_idx + 1]] <- h
  }
  health <- do.call(rbind, healths)
  health[is.finite(Level)]
}


#' Makes a single time series of movement with health status
#'
#'     ID Level      Time Location Event
#' 1:  1     0 0.0000000        2     1
#' 2:  1     0 1.3734866        3     1
#' 3:  1     0 1.3734866        2    -1
combine_health_and_movement <- function(health_dt, movement_dt) {
  health_dt[, "Location"] <- NA
  health_dt[, "Event"] <- 2
  movement_dt[, "Level"] <- NA
  total <- rbind(health_dt, movement_dt)

  total <- total[order(ID, Time, -Event)]
  while (total[, sum(is.na(Level))] > 0) {
    total[, above := shift(Level, 1L, type = "lag")]
    total[is.na(Level), Level := above]
  }
  # We don't need the initial infection event b/c it's an initial
  # state that's already in initial location.
  total <- total[(Event != 2L) | (Time > 0.0)]
  while (total[, sum(is.na(Location))] > 0) {
    total[, above := shift(Location, 1L, type = "lag")]
    total[is.na(Location), Location := above]
  }
  total[, above := NULL]
  total
}


bloodmeal_process <- function(health_dt, movement_dt) {
  movement_from_to <- convert_to_source_destination(movement_dt)
  movement_events <- convert_to_enter_events(movement_from_to)
  health_events <- health_as_events(health_dt)
  movement_health_events <- combine_health_and_movement(health_events, movement_events)
}
