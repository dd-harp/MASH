test_that("module loop works for simplest modules", {
  location_params <- list(people_cnt = 10)
  si_params <- list(recovery_rate = 1 / 200, people_cnt = 10, duration_days = 14)
  bite_params <- list(bite_rate = 1 / 50, people_cnt = 10, duration_days = 14)
  modules <- list(
    location = single_location(location_params),
    health = forced_si_module(si_params),
    bloodmeal = exponential_bite_module(bite_params)
  )
  results <- step_mainloop(modules)
  cat(paste(names(results), "\n"))
  expect_equal(length(results), 3)
})
