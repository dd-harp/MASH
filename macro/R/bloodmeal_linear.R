convert_to_source_destination <- function(location_dt) {
  location_linear <- location_dt[, .(ID = ID, Previous = NA, Location = Start, Time = 0.0)]
  step_cnt <- length(grep("Time", names(location_dt)))
  step_idx <- 1
  location <- paste0("Location", step_idx)
  time <- paste0("Time", step_idx)
  toadd <- location_dt[, .(ID = ID, Location = Location1, Time = Time1)]
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


