test_that("Mosquito RM can be created", {
  patch_cnt <- 5
  params <- queue:::build_biting_parameters(patch_cnt)
  mrm <- Mosquito_RM$new(params)
  expect_equal(2 * 2, 4)
})
