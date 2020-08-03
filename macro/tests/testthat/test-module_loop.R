test_that("module loop works for simplest modules", {
  location_params <- list(people_cnt = 10)
  si_params <- list(recovery_rate = 1 / 200, people_cnt = 10, duration_days = 14)
  bite_params <- list(bite_rate = 1 / 50, people_cnt = 10, duration_days = 14)
  modules <- list(
    location = single_location(location_params),
    health = forced_si_module(si_params),
    bloodmeal = exponential_bite_module(bite_params)
  )
  observer = complete_observer()
  results <- step_mainloop(modules, observer)
  cat(paste(names(results), "\n"))
  expect_equal(length(results), 3)
})

#
# test_that("module loop has slots", {
#   movement <- single_location(location_params)
#   human <- forced_si_module(si_params)
#   biting <- exponential_bite_module(bite_params)
#   record <- especially_human_observer(human)
#   model <- mash_build(location = movement, health = human, bloodmeal = biting, observer = record)
#   model <- step_mainloop(model)
#   observer <- clear_human_observer(record)
#   model <- set_observer(model, observer)
#   model <- step_mainloop(model)
# })
