
test_that("parameters definition is parsable", {
  param_definition <- data.frame(
    name = c("recovery_rate", "people_cnt", "duration_days", "transmission"),
    default = I(c(list(1 / 200), list(100), list(14), list(matrix(c(1,2,3,4), nrow = 2)))),
    type = c("numeric", "integer", "numeric", "matrix[numeric]"),
    description = c(
      "rate per day to recover from malaria infection",
      "number of people in population",
      "time step in days",
      "flow from a to b"
    )
  )
  parameters <- get_parameters(param_definition, list(people_cnt = 10))
  expect_equal(parameters$people_cnt, 10)
  expect_equal(parameters$duration_days, 14)
})
