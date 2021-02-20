library(data.table)


all_movement_locations <- function(location_dt) {
  start_column <- 2
  location_columns <- c(start_column, grep("Location", colnames(location_dt)))
  locations <- unique(as.vector(as.matrix(location_dt[, ..location_columns])))
  locations[is.finite(locations)]
}


test_that("assign levels gives bites", {
  params <- list(day_duration = 1)
  health_rec = data.table::data.table(
    ID = 2,
    Start = 0,
    Time1 = 0.3,
    Level1 = 1,
    Time2 = 2.7,
    Level2 = 0,
    Time3 = 7.3,
    Level3 = 1
  )
  lt <- macro:::assign_levels_to_bites2(
    health_rec, 10, 3, c("Time1", "Time2", "Time3"),
    c("Start", "Level1", "Level2", "Level3"),
    params
    )
  human <- lt$human_level
  bites <- lt$times

  health_rec = data.table::data.table(
    ID = c(2, 2, 2, 2),
    Level = c(0, 1, 0, 1),
    Time = c(0, 0.3, 2.5, 7.3)
  )
  bite_cnt <- 10
  day_idx <- 3
  bite_times <- with(
    params,
    sort(runif(bite_cnt, (day_idx - 1) * day_duration, day_idx * day_duration))
  )
  health_rec$Level[cut(bite_times, health_rec$Time, labels = FALSE)]
  lt2 = macro:::assign_levels_to_bites2(health_rec, 10, 3, params)
})


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
