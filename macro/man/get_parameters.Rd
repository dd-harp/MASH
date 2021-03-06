% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameter_pack.R
\name{get_parameters}
\alias{get_parameters}
\title{Combines given parameters with defaults that have type checks.}
\usage{
get_parameters(param_definition, param_list)
}
\arguments{
\item{param_definition}{A dataframe with columns (name, type, default, description).
The default column is a list of values so that they retain their type.}

\item{param_list}{A list of the settings the user chose.

We need data types so that we can pass to C code and save and load
parameters. This uses that data, specified in a dataframe, in order to
fill out the given parameters with the complete list of defaults.}
}
\description{
Combines given parameters with defaults that have type checks.
}
\examples{
\dontrun{
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
}
}
