# The testthat package reads this file before running tests.
# These functions are used in multiple tests.
within_relative_error <- function(a, b, epsilon) {
    all(abs((a - b) / b) < epsilon)
}

within_absolute_error <- function(a, b, epsilon) {
    all(abs(a - b) < epsilon)
}
