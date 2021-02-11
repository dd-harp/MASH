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


library(data.table)
move_dt <- data.table::data.table(
  id = c(3, 4, 5, 3, 5, 5),
  curtime = c(20, 20, 20, 21.1, 22.3, 22.7),
  curr_location = c(1, 1, 2, 2, 1, 2)
)
basetime <- 20
delta <- 10
loc_cnt <- 3
# initial state is for times at zero, but give leeway to be sure to get 0.0.
state <- unique(move_dt[curtime < basetime + 1e-9,], by = c("id"))
# Adding last moves makes logic easier for accumulation of last dwell time.
last_moves_dt <- move_dt[move_dt[, .I[curtime == max(curtime)], by = id]$V1]
last_moves_dt[, curtime := basetime + delta]
move_all_dt <- rbind(move_dt, last_moves_dt)
# Construct in transpose because we add to one location at a time.
# time index 1 corresponds to duration (basetime, basetime + 1).
dwell.tl <- array(0, dim = c(delta, loc_cnt))

for (row_idx in seq(nrow(move_all_dt))) {
  pid <- move_all_dt[row_idx, id]
  ptime <- move_all_dt[row_idx, curtime]
  ploc <- move_all_dt[row_idx, curr_location]
  dlims <- c(state[id == pid, curtime], ptime)
  lloc <- state[id == pid, curr_location]
  # state[id == pid, curtime := next_time]
  # state[id == pid, curr_location := ploc]
  state[id == pid, `:=`(curtime = ptime, curr_location = ploc)]

  # ceiling because day idx=4 goes from time 3.0 to time 4.0.
  for (idx in seq(ceiling(dlims[1]), ceiling(dlims[2]))) {
    cat(paste("within", idx, dlims[1], dlims[2], "\n"))
    within_day <- min(idx, dlims[2]) -
      max(idx - 1, dlims[1])
    dwell.tl[idx - basetime, lloc] <- dwell.tl[idx - basetime, lloc] + within_day
  }
}
t(dwell.tl)
