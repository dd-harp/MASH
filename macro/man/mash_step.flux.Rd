% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location_flux.R
\name{mash_step.flux}
\alias{mash_step.flux}
\title{Takes one time step of the Flux model}
\usage{
\method{mash_step}{flux}(simulation, step_id, duration_days, health_path)
}
\arguments{
\item{simulation}{A flux model (most likely made via \code{\link[macro]{flux_module}})}

\item{duration_days}{how long to step the simulation for}

\item{health_path}{The history of health states for each person.}
}
\description{
Takes one time step of the Flux model
}
