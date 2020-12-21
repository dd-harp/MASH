function() {
  Rcpp::sourceCpp("R/bloodmeal_rate.cpp")
  # Nobody, so all zero.
  ans = person_exposure(numeric(0), integer(0))
  expect_equal(ans, numeric(10))
  # Arrived after the time, so all zero.
  ans = person_exposure(c(12), c(1))
  ans
  expect_equal(ans, numeric(10))
  # One person at start.
  ans = person_exposure(c(0.0), c(1L))
  ans
  expect_equal(ans, rep(1.0, 10))
  # One person, for half of segment.
  ans = person_exposure(c(0.5), c(1L))
  ans
  expect_equal(ans, c(0.5, rep(1.0, 9)))
  # Error because leave before arrive.
  ans = person_exposure(c(0.5), c(-1L))
  ans
  expect_equal(ans, c(-1, 3.0, 0.5, rep(0.0, 7)))
  # Arrive and leave, whole time.
  ans = person_exposure(c(0, 10), c(1L, -1L))
  ans
  expect_equal(ans, rep(1, 10))

  # Arrive and leave, part
  ans = person_exposure(c(9.5, 10), c(1L, -1L))
  ans
  expect_equal(ans, c(rep(0, 9), 0.5))

  # Arrive two
  ans = person_exposure(c(1.2, 9.5, 10), c(1L, 1L, -1L))
  ans
  expect_equal(ans, c(0, 0.8, rep(1, 7), 1.5))

  ## Assign bites
  Rcpp::sourceCpp("R/bloodmeal_rate.cpp")

  ans = assign_bites(c(1L), c(1), c(1L))
  ans
}
