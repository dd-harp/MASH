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
