% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forced_si.R
\name{human_disease_path.forced_si}
\alias{human_disease_path.forced_si}
\title{Extract the trajectory from the simulation}
\usage{
\method{human_disease_path}{forced_si}(simulation)
}
\arguments{
\item{simulation}{a forced-SI model.}
}
\value{
a list of trajectory entries. Each trajectory entry has the individual's ID,
their initial state as S or I, and times at which they changed state.
}
\description{
Extract the trajectory from the simulation
}
