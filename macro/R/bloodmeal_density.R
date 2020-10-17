#' bloodmeal_density: Bloodmeal for mosquito modules that report mosquito density.
#'
#' The human module and location module both report events for individual humans.
#' The mosquito module reports changes in density of mosquitoes in each location
#' over time.
#'
#' \itemize{
#'   \item \link{bloodmeal_density_module}
#'   \item \link{mash_step.bloodmeal_density}
#'   \item \link{infects_human_path.bloodmeal_density}
#'   \item \link{infects_mosquito_path.bloodmeal_density}
#' }
#'
#' @name bloodmeal_density
#' @docType class
NULL

library(data.table)


#' Converts from incoming wide format data table to a long one.
#' @param location_dt Data table with one individual per row and two columns
#'     for each movement. One column for when, one for where.
#' @return Output data table is a sequence of movements, so columns are
#'     person, time, source, destination.
bld_convert_to_source_destination <- function(location_dt) {
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


#' Takes location movements and turns them into a sequence of entering
#' leaving events for each location.
bld_convert_to_enter_events <- function(location_changes_dt) {
  enter <- location_changes_dt[
    , .(ID = ID, Location = Location, Time = Time, Event = 1)]
  leave <- location_changes_dt[
    is.finite(Previous), .(ID = ID, Location = Previous, Time = Time, Event = -1)]
  events <- rbind(enter, leave)
  events[order(Location, Time)]
}



#' Converts wide data table of health events into a long one.
#' @param health_dt One row per individual, two columns per change.
#'     The two columns are for time and state.
#' @param Output data table is ID, Level, Time, with a row for each
#'     change for each person, plus starting rows for starting states.
bld_health_as_events <- function(health_dt) {
  h0 <- health_dt[, .(ID = ID, Level = Start, Time = 0.0)]
  step_cnt <- length(grep("Time", names(health_dt)))
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
bld_combine_health_and_movement <- function(health_dt, movement_dt) {
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


#' Tells you the state of humans at a location at a given time.
#'
#' @param location_events An event stream of movement and infection events for a single location.
#' @param query_times A length 2 numeric with beginning and ending times for the query.
#' @param previous_state The previously-returned value from this function.
#'
#' This is for assigning outcomes to bites. Each call of this function processes
#' more of the event stream. It returns a data.table that has one entry for
#' each individual in the location at that time, with a Level for each of those
#' individuals. You then take the output from one call and pass it as
#' previous_state for the next call.
#'
#' @export
bld_location_next_state <- function(location_events, query_times, previous_state = NULL) {
  previous_query <- query_times[1]
  if (is.null(previous_state)) {
    previous_state <- location_events[Time == 0.0]
  }
  query_time <- query_times[2]
  loc_before <- location_events[(Time > previous_query) & (Time <= query_time)]
  loc_before <- rbind(previous_state, loc_before)
  present <- loc_before[, Presence := sum(Event), by = ID][Presence == 1]$ID
  loc_before[, Presence := NULL]
  loc_before[ID %in% present][order(-Time), .SD[1], by = ID][, .(ID, Level, Time, Location, Event)]
}


#' Assign bites to humans present at location.
#' Events has columns (ID, Level, Time, Location, Event).
#' Events include enter 1, leave -1, and change infectiousness level 2.
#' Bites has columns (Location, Bite, Time).
#' Returned bites have an ID for the human, that can be -1 for not-a-human,
#' and a Level, for the infectiousness of the human bitten.
bld_bites_at_location <- function(events, bites) {
  previous_time <- 0.0
  previous_state <- NULL
  bites[, `:=`(Level = 0.0, ID = integer(nrow(bites)))]
  for (bite_idx in 1:nrow(bites)) {
    bite_time <- bites[bite_idx]$Time
    bite_level <- bites[bite_idx]$Bite
    human_state <- bld_location_next_state(events, c(previous_time, bite_time), previous_state)

    if (nrow(human_state > 0)) {
      # This samples humans with an equal probability, but this is where we weight it.
      human_idx <- sample(1:nrow(human_state), 1)
      bites[bite_idx, "ID"] <- human_state[human_idx, ID]
      bites[bite_idx, "Level"] <- human_state[human_idx, Level]
    } else {
      # There were no humans at this location to bite, so the bite is lost.
      bites[bite_idx, "ID"] <- -1  # This defines a not-a-human bitten.
      bites[bite_idx, "Level"] <- 0.0
    }

    previous_time <- bite_time
    previous_state <- human_state
  }
  bites
}


bld_bite_outcomes <- function(location_events, bites) {
  locations <- unique(location_events$Location)
  outcomes <- vector(mode = "list", length = length(locations))
  for (out_idx in 1:length(locations)) {
    location = locations[out_idx]
    loc_events <- location_events[Location == location]
    bite_events <- bites[Location == location]
    assigned_bites <- bld_bites_at_location(loc_events, bite_events)
    outcomes[[out_idx]] <- assigned_bites
  }
  do.call(rbind, outcomes)
}


#' Assign bites to people, the top-level function.
#'
#' @param health_dt Health events from the protocol.
#' @param movement_dt Movement events from the protocol.
#' @param bites_dt Bite events from the protocol.
#'
#' infect_human <- outcome_dt[Bite > 0.0]
#' infect_mosquito <- outcome_dt[(Bite == 0.0) & (Level > 0.0)]
#' @export
bld_bloodmeal_process <- function(health_dt, movement_dt, bites_dt) {
  movement_from_to <- bld_convert_to_source_destination(movement_dt)
  movement_events <- bld_convert_to_enter_events(movement_from_to)
  health_events <- bld_health_as_events(health_dt)
  movement_health_events <- bld_combine_health_and_movement(health_events, movement_events)
  bites <- bld_bite_outcomes(movement_health_events, bites_dt)
  bites
}


#' Create a bloodmeal module that assigns the same weight to all humans.
#'
#' @param parameters There are no parameters for this, so this is ignored.
#' @return a module for bloodmeal
#' @export
bloodmeal_density_module <- function(parameters) {
  invisible(parameters)
  module <- list(outcome = NULL)
  class(module) <- "bloodmeal_linear"
  module
}


#' Take one time step for a bloodmeal module.
#'
#' @param simulation The blodmeal module.
#' @param health_dt Human health data.
#' @param movement_dt Movement of humans.
#' @param bites_dt Rate of infectious mosquito bites over time in bites per day.
#' @return Returns the simulation that's updated.
#' @export
mash_step.bloodmeal_density <- function(simulation, health_dt, movement_dt, bites_dt) {
  outcome_dt <- bld_bloodmeal_process(health_dt, movement_dt, bites_dt)
  simulation[["outcome"]] <- outcome_dt
  class(simulation) <- "bloodmeal_linear"
  simulation
}


#' Extract human bites from the bloodmeal module.
#'
#' @param simulation The bloodmeal_linear module.
#' @return A data.table with bite information. These are all bites
#'     of humans where the mosquito was infectious.
#' @export
infects_human_path.bloodmeal_density <- function(simulation) {
  simulation[["outcome"]][Bite > 0]
}


#' Extract mosquito bites from the bloodmeal module.
#'
#' @param simulation The bloodmeal_linear module.
#' @return A data.table with bite information.
#'     These are only bites where the mosquito was not infectious
#'     but the human was.
#' @export
infects_mosquito_path.bloodmeal_density <- function(simulation) {
  simulation[["outcome"]][(Level > 0) & (Bite == 0)]
}
