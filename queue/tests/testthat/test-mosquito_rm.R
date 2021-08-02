test_that("Mosquito RM can be created", {
  patch_cnt <- 5
  parameters <- queue:::build_biting_parameters(patch_cnt)
  mrm <- Mosquito_RM$new(parameters)
  expect_equal(2 * 2, 4)
})

patch_cnt <- 5
parameters <- queue:::build_biting_parameters(patch_cnt)

flog.debug(paste("Human created with parameters ", parameters))
queue:::mrm_check_parameters(parameters)
private <- list()
private$external_parameters <- parameters
private$state = queue:::mosquito_rm_build_biting_state(parameters)
private$parameters <- queue:::build_internal_parameters(parameters)

day_cnt <- parameters$duration
putative_past <- data.table::data.table(
  Location = rep(1:parameters$N, day_cnt),
  Time = rep(0:(parameters$duration - 1), each = parameters$N),
  M = rep(private$state$M, day_cnt),
  Y = rep(rowSums(private$state$Y), day_cnt),
  Z = rep(private$state$Z, day_cnt)
)
stopifnot(is.data.table(putative_past))
putative_past[, c("a") := parameters$a]
# putative_past[, c("a") := list(parameters$a)]
# putative_past[, `:=`(a = parameters$a)]
private$output <- putative_past
