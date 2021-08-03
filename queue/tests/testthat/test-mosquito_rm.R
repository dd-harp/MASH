test_that("Mosquito RM can be created", {
  patch_cnt <- 5
  parameters <- queue:::build_biting_parameters(patch_cnt)
  mrm <- Mosquito_RM$new(parameters)
  bloodmeal_dt <- queue:::sample_mosquito_kappa(patch_cnt, parameters$duration)

  params <- list(
    lambda = rep(5, patch_cnt),
    duration = parameters$duration
  )
  ap = Aquatic_Poisson$new(params)
  ap$step(9)
  aquatic <- ap$path()

  mrm$step(1, bloodmeal_dt, aquatic)
  output <- mrm$path()
  expect_equal(nrow(output), patch_cnt * parameters$duration)
  expect_true(setequal(names(output),
                       c("Location", "Time", "M", "Y", "Z", "a")))
})
