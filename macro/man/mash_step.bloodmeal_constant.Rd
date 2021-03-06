% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bloodmeal_constant.R
\name{mash_step.bloodmeal_constant}
\alias{mash_step.bloodmeal_constant}
\title{Take one time step for a bloodmeal module.}
\usage{
\method{mash_step}{bloodmeal_constant}(simulation, step_id, health_dt, movement_dt, bites_dt)
}
\arguments{
\item{simulation}{The blodmeal module.}

\item{health_dt}{Human health data.}

\item{movement_dt}{Movement of humans.}

\item{bites_dt}{Rate of infectious mosquito bites over time in bites per day.}
}
\value{
Returns the simulation that's updated.
}
\description{
Take one time step for a bloodmeal module.
}
