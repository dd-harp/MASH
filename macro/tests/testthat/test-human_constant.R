test_that("human_constant can create", {
  module <- human_constant_module(list(people_cnt = 20, duration_days = 10, initial_pfpr=0.4))
  expect_true(class(module) == "human_constant")
})


test_that("human_constant increments time", {
  module <- human_constant_module(list(people_cnt = 20, duration_days = 10, initial_pfpr=0.4))
  module <- mash_step(module, "bites not really used")
  expect_lt(abs(module$time - 10), 1e-11)
  module <- mash_step(module, "bites not really used")
  expect_lt(abs(module$time - 20), 1e-11)
})


test_that("human_constant events don't change", {
  module <- human_constant_module(list(people_cnt = 20, duration_days = 10, initial_pfpr=0.4))
  events <- human_disease_path(module)
  expect_equal(nrow(events), 20)
  expect_true(setequal(colnames(events), c("ID", "Time", "Level")))
})
