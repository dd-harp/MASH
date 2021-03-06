% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location_flux.R
\name{flux_transitions}
\alias{flux_transitions}
\title{Movement: Make 'Flux' Transitions}
\usage{
flux_transitions()
}
\value{
a list of transitions
}
\description{
This function makes the set of transitions for the 'flux' model of movement;
the simple trip model is the simplest Eulerian movement model, meaning that it
individuals are indistinguishable. Because of this, for \eqn{N} patches, the state
is a vector of length \eqn{N} where each element gives the number of individuals there.
}
