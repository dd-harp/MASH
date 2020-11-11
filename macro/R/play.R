
Rcpp::sourceCpp("R/bloodmeal_rate.cpp")
person_exposure()
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
