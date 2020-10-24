library(data.table)


all_movement_locations <- function(location_dt) {
  start_column <- 2
  location_columns <- c(start_column, grep("Location", colnames(location_dt)))
  locations <- unique(as.vector(as.matrix(location_dt[, ..location_columns])))
  locations[is.finite(locations)]
}


test_that("bloodmeal processes sample data", {
  human_cnt <- 10L
  place_cnt <- 5L
  bite_cnt <- 20L
  time_step <- 10.0
  human_events <- macro:::sample_humans_at_location()
  bites_dt <- macro:::sample_mosquito_kappa()
  result <- macro:::bld_bites_at_location(human_events, bites_dt)
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
  # all bites in places that had people are returned.
  all_locations <- all_movement_locations(move_dt)
  bites_with_people <- bite_dt[Location %in% all_locations,]
  expect_equal(nrow(bites_with_people), nrow(outcome_dt))

  expect_true(bite_dt[Location == 3, all(Bite == 1)])
  for (check_loc in c(1, 2, 4, 5)) {
    expect_true(bite_dt[Location == check_loc, all(Bite == 0.0)])
  }
})


test_that("bloodmeal_linear runs as a module", {
  human_cnt <- 10L
  place_cnt <- 5L
  bite_cnt <- 20L
  time_step <- 10.0
  parameters <- list()
  simulation <- bloodmeal_linear_module(parameters)
  for (i in 1:4) {
    current_time <- time_step * (i - 1)

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
