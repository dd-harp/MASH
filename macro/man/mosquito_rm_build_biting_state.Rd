% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mosquito_rm.R
\name{mosquito_rm_build_biting_state}
\alias{mosquito_rm_build_biting_state}
\title{Create a default state for mosquito-RM model.}
\usage{
mosquito_rm_build_biting_state(infectious, replacement, maxEIP)
}
\arguments{
\item{infectious}{An array of the count of infectious mosquitoes for each patch.}

\item{replacement}{The fraction of infectious that should be in incubating for each day.
A good choice for this is the complement of the daily conditional survival.}

\item{maxEIP}{The maximum length of the EIP for any day of the year, in days.}
}
\value{
A list of the main variables.
\itemize{
\item \eqn{M} is adult females. This will be an array with a float for each patch.
\item \eqn{Y} are incubating mosquitoes, one for each patch, and one for each day
of the EIP, so it's a matrix (patch x EIP day). The first column is newly-hatched.
The last column are those incubating mosquitoes that are beyond the EIP.
\item \eqn{Z} are infectious mosquitoes.
\item \code{simulation_day} This is the count of the number of days into the simulation.
}
}
\description{
Create a default state for mosquito-RM model.
}