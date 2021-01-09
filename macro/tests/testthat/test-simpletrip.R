# --------------------------------------------------------------------------------
#
#   tests for Simple Trip model
#   April 2020
#
# --------------------------------------------------------------------------------

test_that("simple trip model matches theoretical expectations", {

  parameters <- new.env()
  parameters$trip_rate <- c(1/14,1/21)
  parameters$trip_dest <- matrix(c(0,1,1,0),byrow = TRUE,nrow = 2,ncol = 2)
  parameters$return_home_rate <- matrix(c(0,1/5,1/3,0),byrow = TRUE,nrow = 2,ncol = 2)
  parameters$npatch <- 2
  parameters$duration_days <- 10

  tau12 <- parameters$return_home_rate[1,2]
  tau21 <- parameters$return_home_rate[2,1]
  phi12 <- parameters$trip_rate[1] * parameters$trip_dest[1,2]
  phi21 <- parameters$trip_rate[2] * parameters$trip_dest[2,1]

  population <- data.table::data.table(
    who = 1:2,
    home = 1:2,
    current = 1:2
  )

  transitions <- simple_trip_transitions()

  simulation <- continuous_simulation(
    population,
    transitions,
    simple_trip_observer,
    parameters
  )

  simulation <- init_continuous(simulation)
  simulation <- run_continuous(simulation,2e4)

  trajectory <- Filter(Negate(is.null),simulation$trajectory)
  trajectory <- do.call(rbind,trajectory)
  trajectory <- data.frame(
    name=unlist(trajectory[,"name"]),
    curtime=unlist(trajectory[,"curtime"]),
    id=unlist(trajectory[,"id"]),
    curr_location=unlist(trajectory[,"curr_location"]),
    prev_location=unlist(trajectory[,"prev_location"])
  )

  avg_state <- macro:::simple_trip_stateoutput(trajectory,"S3")

  pivec <- rep(0,4)
  pivec[1] <- (phi21*tau12)/((phi12+tau12)*(phi21+tau21))
  pivec[2] <- (phi12*phi21)/((phi12+tau12)*(phi21+tau21))
  pivec[3] <- (tau12*tau21)/((phi12+tau12)*(phi21+tau21))
  pivec[4] <- (phi12*tau21)/((phi12+tau12)*(phi21+tau21))

  tst <- stats::chisq.test(x = avg_state, p = pivec, simulate.p.value = TRUE)
  res <- tst$p.value > 0.99
  expect_true(res)
})
