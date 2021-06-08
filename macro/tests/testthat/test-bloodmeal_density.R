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
  time_id <- list(time = 0, duration = params$duration)
  bloodmeal <- mash_step(
      bloodmeal, time_id, health_dt, movement_dt, mosquito_dt)
  human_infections <- infects_human_path(bloodmeal)
  mosquito_infections <- infects_mosquito_path(bloodmeal)
  expect_equal(nrow(mosquito_infections), params$day_cnt * params$location_cnt)
  expect_equal(colnames(human_infections), c("times", "human"))
})
