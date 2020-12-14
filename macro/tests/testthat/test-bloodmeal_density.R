library(data.table)


all_movement_locations <- function(location_dt) {
  start_column <- 2
  location_columns <- c(start_column, grep("Location", colnames(location_dt)))
  locations <- unique(as.vector(as.matrix(location_dt[, ..location_columns])))
  locations[is.finite(locations)]
}


test_that("bloodmeal density assigns bites at location", {
  human_cnt <- 10L
  place_cnt <- 5L
  time_step <- 10.0
  health_dt <- sample_health_infection_status(human_cnt, time_step)
  move_dt <- sample_move_location(human_cnt, place_cnt, time_step)
  bite_dt <- sample_mosquito_kappa(place_cnt, time_step)
  movement_from_to <- macro:::bld_convert_to_source_destination(move_dt)
  movement_events <- macro:::bld_convert_to_enter_events(movement_from_to)
  health_events <- macro:::bld_health_as_events(health_dt)
  movement_health_events <- macro:::bld_combine_health_and_movement(health_events, movement_events)

})


test_that("bloodmeal density processes sample data", {
  human_cnt <- 10L
  place_cnt <- 5L
  time_step <- 10.0
  health_dt <- sample_health_infection_status(human_cnt, time_step)
  move_dt <- sample_move_location(human_cnt, place_cnt, time_step)
  bite_dt <- sample_mosquito_kappa(place_cnt, time_step)
  outcome_dt <- bld_bloodmeal_process(health_dt, move_dt, bite_dt)
  # all bites in places that had people are returned.
  all_locations <- all_movement_locations(move_dt)
  bites_with_people <- bite_dt[Location %in% all_locations,]
  expect_equal(nrow(bites_with_people), nrow(outcome_dt))
})