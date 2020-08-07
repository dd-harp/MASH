library(data.table)


test_that("bloodmeal processes sample data", {
  human_cnt <- 10L
  place_cnt <- 5L
  bite_cnt <- 20L
  time_step <- 10.0
  health_dt <- sample_health_infection_status(human_cnt, time_step)
  move_dt <- sample_move_location(human_cnt, place_cnt, time_step)
  bite_dt <- sample_mosquito_half_bites(bite_cnt, place_cnt, time_step)
  outcome_dt <- bloodmeal_process(health_dt, move_dt, bite_dt)
  expect_equal(nrow(bite_dt), nrow(outcome_dt))
})



test_that("bloodmeal all infectious bites", {
  human_cnt <- 10L
  place_cnt <- 5L
  bite_cnt <- 20L
  time_step <- 10.0
  health_dt <- sample_health_infection_status(human_cnt, time_step)
  move_dt <- sample_move_location(human_cnt, place_cnt, time_step)
  bite_dt <- sample_mosquito_half_bites(bite_cnt, place_cnt, time_step)
  bite_dt[, "Bite"] <- 0.0
  bite_dt[Location == 3, "Bite"] <- 1.0
  outcome_dt <- bloodmeal_process(health_dt, move_dt, bite_dt)
  expect_equal(nrow(bite_dt), nrow(outcome_dt))
  expect_true(bite_dt[Location == 3, all(Bite == 1)])
  for (check_loc in c(1, 2, 4, 5)) {
    expect_true(bite_dt[Location == check_loc, all(Bite == 0.0)])
  }
})


test_that("bloodmeal_linear runs as a module", {
  duration_days <- 10.0
  parameters <- list()
  simulation <- bloodmeal_linear_module(parameters)
  for (i in 1:4) {
    current_time <- duration_days * (i - 1)

    health_dt <- sample_health_infection_status(human_cnt, time_step)
    move_dt <- sample_move_location(human_cnt, place_cnt, time_step)
    bite_dt <- sample_mosquito_half_bites(bite_cnt, place_cnt, time_step)

    stepped <- mash_step(simulation, health_dt, move_dt, bite_dt)
    human_bites <- infects_human_path(stepped)
    mosquito_bites <- infects_mosquito_path(stepped)
    expect_gt(nrow(human_bites), 0)
    expect_gt(nrow(mosquito_bites), 0)
  }
})
