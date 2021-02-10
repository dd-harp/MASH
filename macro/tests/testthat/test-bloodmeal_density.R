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
    dispersion = 1.5,
    day_cnt = 10,
    biting_weight = 0.5
  )
  day_start <- 1
  health_dt <- sample_health_infection_status(params$human_cnt, params$duration)
  bite_weight <- runif(params$human_cnt, 0.2, 0.8)
  movement_dt <- sample_move_location(
      params$human_cnt, params$location_cnt, params$duration)
  mosquito_dt <- sample_mosquito_myz(params$location_cnt, params$duration)

  response = bld_bloodmeal_process(
    health_dt, movement_dt, mosquito_dt, day_start, params)
  mosquito_infections <- response[[1]]
  human_infections <- response[[2]]
  expect_equal(nrow(mosquito_infections), params$day_cnt * params$location_cnt)
  expect_equal(colnames(human_infections), c("times", "human"))
})


test_that("bloodmeal density processes sample data", {
  params <- list(
    human_cnt = 8L,
    location_cnt = 5L,
    duration = 10,
    day_duration = 1,
    day_start = 1,
    dispersion = 1.5,
    day_cnt = 10,
    biting_weight = 0.5
  )
  health_dt <- sample_health_infection_status(params$human_cnt, params$duration)
  bite_weight <- runif(params$human_cnt, 0.2, 0.8)
  movement_dt <- sample_move_location(
      params$human_cnt, params$location_cnt, params$duration)
  mosquito_dt <- sample_mosquito_myz(params$location_cnt, params$duration)

  bloodmeal <- bloodmeal_density_module(params)
  bloodmeal <- mash_step(bloodmeal, health_dt, movement_dt, mosquito_dt)
  human_infections <- infects_human_path(bloodmeal)
  mosquito_infections <- infects_mosquito_path(bloodmeal)
  expect_equal(nrow(mosquito_infections), params$day_cnt * params$location_cnt)
  expect_equal(colnames(human_infections), c("times", "human"))
})


test_that("single_dwell translates events to dwell times", {
  location_cnt <- 10L
  step_duration <- 10.0
  human_cnt <- 8L
  place_cnt <- 5L
  time_step <- 10.0
  move_dt <- sample_move_location(human_cnt, place_cnt, time_step)
  day_cnt <- as.integer(step_duration / 1)
  move_cnt <- length(grep("Time", names(move_dt)))
  for (h_idx in 1:nrow(move_dt)) {
    h_record <- move_dt[h_idx, ]
    dwell.lt <- macro:::single_dwell(
        h_record, location_cnt, move_cnt, 1, step_duration)
    expect_true(all(abs(colSums(dwell.lt) - 1) < 1e-10))
    expect_equal(dim(dwell.lt), c(location_cnt, day_cnt))
  }
})


test_that("single_dwell handles times past first events", {
  location_cnt <- 10L
  step_duration <- 10.0
  human_cnt <- 8L
  place_cnt <- 5L
  time_step <- 10.0
  move_dt <- sample_move_location(human_cnt, place_cnt, time_step)
  col_cnt <- length(colnames(move_dt))
  time_cnt <- (col_cnt - 2) / 2
  for (i in 1:time_cnt) {
    colname <- paste0("Time", i)
    move_dt[, colname] <- move_dt[, colname, with=FALSE] + 10
  }
  day_cnt <- as.integer(step_duration / 1)
  move_cnt <- length(grep("Time", names(move_dt)))
  h_idx <- 1
  h_record <- move_dt[move_dt$ID == 7, ]
  dwell.lt <- macro:::single_dwell(
    h_record, location_cnt, move_cnt, 10, step_duration)
  expect_true(all(colSums(dwell.lt) == 1))
  expect_equal(dim(dwell.lt), c(location_cnt, day_cnt))
})
