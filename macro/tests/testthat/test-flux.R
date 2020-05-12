# --------------------------------------------------------------------------------
#
#   tests for Flux model
#   May 2020
#
# --------------------------------------------------------------------------------

test_that("flux model matches theoretical expectations", {

  parameters <- new.env()
  parameters$rate_matrix <- matrix(c(0,1/6,1/5,0),byrow = TRUE,nrow = 2,ncol = 2)
  parameters$npatch <- 2
  parameters$location <- 1:2

  simulation <- flux_module(parameters)
  simulation <- mash_step(simulation, 2e4, NULL)

  trajectory <- Filter(Negate(is.null),simulation$trajectory)
  trajectory <- do.call(rbind,trajectory)

  state_occupancy <- flux_stateoutput(trajectory,"S1")

  f_12 <- parameters$rate_matrix[1,2]
  f_21 <- parameters$rate_matrix[2,1]

  P_stationary <- rep(0,3)
  P_stationary[1] = (2*f_12*f_21) / ((f_12 + f_21)^2)
  P_stationary[2] = (f_21^2) / ((f_12 + f_21)^2)
  P_stationary[3] = (f_12^2) / ((f_12 + f_21)^2)

  tst <- stats::chisq.test(x = state_occupancy, p = P_stationary, simulate.p.value = TRUE)
  res <- tst$p.value > 0.99
  expect_true(res)
})
