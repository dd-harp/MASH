library(data.table)


all_movement_locations <- function(location_dt) {
  start_column <- 2
  location_columns <- c(start_column, grep("Location", colnames(location_dt)))
  locations <- unique(as.vector(as.matrix(location_dt[, ..location_columns])))
  locations[is.finite(locations)]
}


test_that("bloodmeal density assigns bites at location", {
  params <- list(
    human_cnt = 8L,
    location_cnt = 5L,
    duration = 10,
    day_duration = 1,
    dispersion = 1.5
  )
  day_cnt <- as.integer(params$duration / params$day_duration)
  health_dt <- sample_health_infection_status(params$human_cnt, params$duration)
  bite_weight <- runif(params$human_cnt, 0.2, 0.8)
  movement_dt <- sample_move_location(params$human_cnt, params$location_cnt, params$duration)
  mosquito_dt <- sample_mosquito_myz(params$location_cnt, params$duration)
  dwell.lh <- macro:::human_dwell(movement_dt, params)
  M_arr <- macro:::data_table_to_array(mosquito_dt, "Location", "Time", "M")
  Z_arr <- macro:::data_table_to_array(mosquito_dt, "Location", "Time", "Z")
  biting_arr <- macro:::data_table_to_array(mosquito_dt, "Location", "Time", "a")
  day_idx <- 1
  bites.lh <- macro:::sample_bites(
    dwell.lh[,, day_idx], M_arr[,day_idx], biting_arr[, day_idx],
    bite_weight, params)
  infectious_to_mosquito.lt <- array(0, dim = c(params$location_cnt, day_cnt))
  # This is for each entry in the matrix
  time_cols <- grep("Time", colnames(health_dt))
  level_cols <- c(time_cols[1] - 1, time_cols + 1)
  l_idx <- 1
  h_idx <- 1
  bite_cnt <- bites.lh[l_idx, h_idx]
  health_rec <- health_dt[health_dt$ID == h_idx,]
  human_status <- macro:::assign_levels_to_bites(health_rec, bite_cnt, time_cols, level_cols, params)

})


test_that("bloodmeal density processes sample data", {
  human_cnt <- 10L
  place_cnt <- 5L
  time_step <- 10.0
  health_dt <- sample_health_infection_status(human_cnt, time_step)
  move_dt <- sample_move_location(human_cnt, place_cnt, time_step)
  bite_dt <- sample_mosquito_myz(place_cnt, time_step)
})
