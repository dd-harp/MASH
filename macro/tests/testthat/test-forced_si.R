#' Make sure the infection transitions work by testing them
#' against a single individual.
test_that("forced si individual infect", {
  transition <- forced_si_infect()
  empty_state <- list(disease = "S", bites = list(numeric(0)))
  expect_false(is_enabled(transition, empty_state, 0))

  state <- list(disease = "S", bites = list(c(0.1, 1.8, 1.9)))
  expect_true(is_enabled(transition, state, 0))
  expect_true(is_enabled(transition, state, 1.5))
  expect_false(is_enabled(transition, state, 2))

  infected <- list(disease = "I", bites = list(c(0.1, 1.8, 1.9)))
  expect_false(is_enabled(transition, infected, 0))
  expect_false(is_enabled(transition, infected, 1.5))
  expect_false(is_enabled(transition, infected, 2))

  expect_equal(when(transition, state, 0), 0.1)
  expect_equal(when(transition, state, 0.1001), 1.8)
  expect_equal(when(transition, state, 2), Inf)

  # Note that firing doesn't go back and check the bite times.
  fire_out <- fire(transition, state, 222)
  expect_equal(fire_out$disease, "I")
  expect_equal(fire_out$bites, state$bites)
})


#' Test the recovery transitions by testing them against a single
#' individual.
test_that("forced si individual recover", {
  transition <- forced_si_recover(1.0)
  state <- list(disease = "I", bites = list(c(0.1, 1.8, 1.9)))
  expect_true(is_enabled(transition, state, 0))
  expect_true(is_enabled(transition, state, 2))

  s_state <- list(disease = "S", bites = list(c(0.1, 1.8, 1.9)))
  expect_false(is_enabled(transition, s_state, 0))

  expect_gt(when(transition, state, 0), 0)

  # Note that firing doesn't go back and check the bite times.
  fire_out <- fire(transition, state, 222)
  expect_equal(fire_out$disease, "S")
  expect_equal(fire_out$bites, state$bites)
})


test_that("forced si example runs", {
  set.seed(234243)
  transitions <- list(
    infect = forced_si_infect(),
    recover = forced_si_recover(1 / 200)
  )
  people_cnt <- 100
  pfpr <- 0.4
  individuals <- forced_si_population(people_cnt, pfpr)
  bite_rate <- 1 / 20
  duration_days <- 14
  individuals[, "bites"] <- forced_si_create_bites(people_cnt, bite_rate, 0, duration_days)

  simulation <- continuous_simulation(
    individuals,
    transitions,
    forced_si_observer
    )
  simulation <- init_simulation(simulation)
  simulation <- run_simulation(simulation, duration_days)
  trajectory <- simulation$trajectory[1:simulation$trajectory_cnt]
  expect_gt(length(trajectory), 1)
})
