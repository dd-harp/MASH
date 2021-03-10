#' bloodmeal_density: Bloodmeal for mosquito modules that report mosquito density.
#'
#' The human module and location module both report events for individual humans.
#' The mosquito module reports changes in density of mosquitoes in each location
#' over time. The mosquito module doesn't report a number of bites at a time
#' but does report a biting rate over time. This assigns bites out of the
#' biting rate and the available biting weight of humans.
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


integrated_person_days <- function(when, events) {
  person_exposure(when, events)
}



#' Iterate over locations and calculate bloodmeal for each location
#' location_events has columns `ID`, `Level`, `Time`, `Location`, `Event`.
#' Level is 0 for uninfectious, 1 for infectious. Event is 1, -1 for enter
#' and leave, 2 for change in infectious status.
bld_bite_outcomes <- function(location_events, bites) {
  locations <- unique(location_events$Location)
  human_outcomes <- vector(mode = "list", length = length(locations))
  mosquito_outcomes <- vector(mode = "list", length = length(locations))
  for (out_idx in 1:length(locations)) {
    location = locations[out_idx]
    loc_events <- location_events[Location == location]
    bite_events <- bites[Location == location]
    assigned_bites <- bld_bites_at_location(loc_events, bite_events)
    human_outcomes[[out_idx]] <- assigned_bites
  }
  list(
    human = do.call(rbind, human_outcomes),
    mosquito = do.call(rbind, mosquito_outcomes)
  )
}


#' Given a data table, generate an array form.
#' @param dt the data table
#' @param row the name of the column to use as row values. Must be integers.
#' @param col name of column to use as column values. Must be integers.
#' @param value name of column to use as values.
#' @return an array with size from min to max in row and column, assuming
#'     they are integers that are in order.
data_table_to_array <- function(dt, row, col, value) {
  row_min <- min(dt[[row]])
  row_cnt <- max(dt[[row]]) - row_min + 1
  col_min <- min(dt[[col]])
  col_cnt <- max(dt[[col]]) - col_min + 1

  arr <- array(0, dim = c(row_cnt, col_cnt))
  row_offset <- 1 - row_min
  col_offset <- 1 - col_min
  for (row_idx in 1:nrow(dt)) {
    rec <- dt[row_idx,]
    arr[rec[[row]] + row_offset, rec[[col]] + col_offset] <- rec[[value]]
  }
  arr
}


#' Given all human movement, return a matrix of fraction of time at each
#' location on each day.
#' @param movement_dt The movement data table where each row is an event.
#' @param params A list or data frame that has simulation parameters such
#'     as location_cnt and duration of the time step.
#' @return A three-dimensional array, location x human x days, so that
#'     we can process a day at a time.
human_dwell <- function(movement_dt, day_start, params) {
  stopifnot(c("Location", "Time", "ID") %in% colnames(movement_dt))
  location_cnt <- params$location_cnt
  step_duration <- params$duration

  day_cnt <- as.integer(round(step_duration / 1))
  human_cnt <- length(unique(movement_dt$ID))

  # initial state is for times at zero, but give leeway to be sure to get 0.0.
  state <- unique(movement_dt[Time < day_start + 1e-9,], by = c("ID"))
  # Adding last moves makes logic easier for accumulation of last dwell time.
  last_moves_dt <- movement_dt[movement_dt[, .I[Time == max(Time)], by = ID]$V1]
  last_moves_dt[, Time := day_start + day_cnt]
  move_all_dt <- rbind(movement_dt, last_moves_dt)
  # Construct in transpose because we add to one location at a time.
  # time index 1 corresponds to duration (day_start, day_start + 1).
  dwell.tlh <- array(0, dim = c(day_cnt, location_cnt, human_cnt))

  for (row_idx in seq(nrow(move_all_dt))) {
    pid <- move_all_dt[row_idx, ID]
    ptime <- move_all_dt[row_idx, Time]
    ploc <- move_all_dt[row_idx, Location]
    dlims <- c(state[ID == pid, Time], ptime)
    lloc <- state[ID == pid, Location]
    state[ID == pid, `:=`(Time = ptime, Location = ploc)]

    for (idx in seq(ceiling(dlims[1]), ceiling(dlims[2]))) {
      within_day <- min(idx, dlims[2]) - max(idx - 1, dlims[1])
      didx <- idx - day_start
      dwell.tlh[didx, lloc, pid] <- dwell.tlh[didx, lloc, pid] + within_day
    }
  }
  # return (location, human, day)
  aperm(dwell.tlh, c(2, 3, 1))
}


#' Assign bite levels for bites of a single human.
#' @param health_dt Data for just this human from the health table.
#' @param bite_cnt The number of bites to create in day `day_idx`.
#' @param The day within the duration. Day 1 starts at time 0.
#' @param params Has a `day_duration` which is usually 1.
assign_levels_to_bites <- function(health_dt, bite_cnt, day_idx, params) {
  bite_times <- sort(runif(bite_cnt, day_idx - 1, day_idx))
  base_time <- min(health_dt$Time)
  # Ensure there is one break past the end and rescale times to 0-10.
  breaks <- c(health_dt$Time - base_time, params$duration + 1e-7)
  levels <- health_dt$Level[cut(bite_times, breaks, labels = FALSE)]
  stopifnot(all(!is.na(levels)))
  data.table::data.table(
    human_level = levels,
    times = bite_times
  )
}


#' Given a number of bites, assign them to classes of mosquitoes.
#' @param bites_df has `human_level` and `times` for each bite
#' @param M is number of adult females
#' @param Y is number of exposed adult females
#' @param Z is number of infectious adult females
#' @return adds `infect_mosquito` and `infect_human` to the data frame.
assign_mosquito_status <- function(bites_df, M, Y, Z) {
  if (M > 0) {
    mosy_classes <- c(M - Y - Z, Y, Z)
    if (any(mosy_classes < 0)) {
      logdebug("negative M-Y-Z")
      mosy_classes <- abs(mosy_classes)
    }
    category <- rmultinom(nrow(bites_df), 1, mosy_classes)
    bites_df$infect_mosquito = category[1, ] & bites_df$human_level
    bites_df$infect_human = category[3, ]
  } else {
    bites_df$infect_mosquito <- 0
    bites_df$infect_human <- 0
  }
  bites_df
}


#' Assign bites to humans and mosquitoes for one day.
#'
#' @param day_idx Index of day within the time step.
#' @param M_arr Matrix of location x day for total number of mosquitoes.
#' @param Y_arr Matrix of location x day for incubating mosquitoes.
#' @param Z_arr Matrix of location x day for infectious mosquitoes.
#' @param biting_arr Sample bites from this, location x day.
#' @param dwell.lh Total dwell time in each location by humans,
#'     location x human.
#' @param bite_weight Biting weight of each human.
#' @param health_dt Health status from the human module.
#' @param params Module parameters.
#'
#' This function samples bites in accordance with both the human
#' biting weight and the mosquito biting rate.
bld_single_day <- function(
      day_idx, M_arr, Y_arr, Z_arr, biting_arr, dwell.lh, bite_weight,
      health_dt, params
      ) {
  M_day <- M_arr[, day_idx]
  Y_day <- Y_arr[, day_idx]
  Z_day <- Z_arr[, day_idx]
  bites.lh <- sample_bites(
    dwell.lh[,, day_idx], M_arr[,day_idx], biting_arr[, day_idx],
    bite_weight, params)
  infectious_to_mosquito.lt <- array(
      0, dim = c(params$location_cnt, params$day_cnt))
  # This is for each entry in the matrix
  time_cols <- grep("Time", colnames(health_dt))
  level_cols <- c(time_cols[1] - 1, time_cols + 1)
  lh_df <- expand.grid(1:params$location_cnt, 1:params$human_cnt)
  # Produces a list where each item is a list with mosquito infections
  # and human infections
  combined_df <- lapply(
    1:nrow(lh_df),
    function(lh_idx) {
      l_idx <- lh_df[lh_idx, 1]
      h_idx <- lh_df[lh_idx, 2]
      bite_cnt <- bites.lh[l_idx, h_idx]
      health_rec <- health_dt[health_dt$ID == h_idx,]
      human_status <- assign_levels_to_bites(
          health_rec, bite_cnt, day_idx, params)
      with_mosquito <- assign_mosquito_status(
          human_status, M_day[l_idx], Y_day[l_idx], Z_day[l_idx])
      human_infections <- with_mosquito[
          with_mosquito$infect_human > 0, c("times")]
      human_infections$human <- h_idx
      list(
        mosquito_infections = data.frame(
          location = l_idx,
          human = h_idx,
          mosquito_infections = sum(with_mosquito$infect_mosquito),
          time = day_idx - 1
        ),
        human_infections = human_infections
      )
    })
  mosquitos_by_human <- do.call(rbind, lapply(combined_df, function(x) x[[1]]))
  infections_in_location <- aggregate(
    mosquitos_by_human$mosquito_infections, list(mosquitos_by_human$location),
    FUN=sum)
  colnames(infections_in_location) <- c("location", "mosquito_infections")
  infections_in_location$time <- (day_idx - 1) * params$day_duration
  list(
    mosquito_infections = infections_in_location,
    human_infections = do.call(rbind, lapply(combined_df, function(x) x[[2]]))
  )
}


#' Assign bites to people, the top-level function.
#'
#' @param health_dt Health events from the protocol.
#' @param movement_dt Movement events from the protocol.
#' @param mosquito_dt Mosquito levels.
#' @param day_start The time at which the first day starts, so 0.0 or 10.0,
#'     not 1L or 11L.
#' @param params Has `human_cnt`, `location_cnt`, `duration`,
#'     `day_duration`, `dispersion`, `day_cnt`, `biting_weight`.
#' infect_human <- outcome_dt[Bite > 0.0]
#' infect_mosquito <- outcome_dt[(Bite == 0.0) & (Level > 0.0)]
#' @export
bld_bloodmeal_process <- function(
    health_dt, movement_dt, mosquito_dt, day_start, params
    ) {
  stopifnot("biting_weight" %in% names(params))
  if (length(params$biting_weight) == 1) {
    bite_weight <- rep(params$biting_weight, params$human_cnt)
  } else {
    stopifnot(length(params$biting_weight) == params$human_cnt)
    bite_weight <- params$biting_weight
  }
  dwell.lh <- human_dwell(movement_dt, day_start, params)
  stopifnot(dim(dwell.lh)[2] == params$params$human_cnt)
  logdebug(paste("dwell.lh dims", paste0(dim(dwell.lh), collapse=",")))
  M_arr <- data_table_to_array(mosquito_dt, "Location", "Time", "M")
  Y_arr <- data_table_to_array(mosquito_dt, "Location", "Time", "Y")
  Z_arr <- data_table_to_array(mosquito_dt, "Location", "Time", "Z")
  biting_arr <- data_table_to_array(mosquito_dt, "Location", "Time", "a")

  days_list <- lapply(
    1:params$day_cnt,
    function(day_idx) bld_single_day(day_idx, M_arr, Y_arr, Z_arr, biting_arr,
                                     dwell.lh, bite_weight, health_dt, params)
  )
  # day_list has an entry for each day. Each entry is a list of two dataframes.
  mosquito_events <- data.table::data.table(
    do.call(rbind, lapply(days_list, function(x) x[[1]])))
  mosquito_events[, `:=`(time = time + day_start - 1)]
  human_events <- data.table::data.table(
    do.call(rbind, lapply(days_list, function(x) x[[2]])))
  human_events[, `:=`(times = times + day_start - 1)]

  list(mosquito_events = mosquito_events, human_events = human_events)
}


#' Create a bloodmeal module that assigns the same weight to all humans.
#'
#' @param parameters There are no parameters for this, so this is ignored.
#' @return a module for bloodmeal
#' @export
bloodmeal_density_module <- function(parameters) {
  module <- list(
    parameters = parameters,
    day_start = parameters$day_start,
    mosquito_events = NULL,
    human_events = NULL
    )
  class(module) <- "bloodmeal_density"
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
mash_step.bloodmeal_density <- function(
  simulation, step_id, health_dt, movement_dt, bites_dt
  ) {
  outcome <- bld_bloodmeal_process(
    health_dt, movement_dt, bites_dt, simulation$day_start,
    simulation[["parameters"]]
    )
  simulation$mosquito_events <- outcome$mosquito_events
  simulation$human_events <- outcome$human_events
  simulation$day_start <- simulation$day_start + simulation$parameters$day_cnt
  class(simulation) <- "bloodmeal_density"
  simulation
}


#' Extract human bites from the bloodmeal module.
#'
#' @param simulation The bloodmeal_density module.
#' @return A data.table with bite information. These are all bites
#'     of humans where the mosquito was infectious.
#' @export
infects_human_path.bloodmeal_density <- function(simulation) {
  simulation$human_events
}


#' Extract mosquito bites from the bloodmeal module.
#'
#' @param simulation The bloodmeal_density module.
#' @return A data.table with bite information.
#'     These are only bites where the mosquito was not infectious
#'     but the human was.
#' @export
infects_mosquito_path.bloodmeal_density <- function(simulation) {
  data.table::setnames(simulation$mosquito_events, "location", "Location")
  data.table::setnames(
    simulation$mosquito_events, "mosquito_infections", "Bites")
  data.table::setnames(simulation$mosquito_events, "time", "Time")
  simulation$mosquito_events
}
