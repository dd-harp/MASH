% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location_simpletrip.R
\name{person_path.simple_trip}
\alias{person_path.simple_trip}
\title{Return trajectory by person for Simple Trip model}
\usage{
\method{person_path}{simple_trip}(simulation)
}
\arguments{
\item{simulation}{A simple trip model (most likely made via \code{\link[macro]{simple_trip_module}})}
}
\value{
a list of \code{data.frame}, one for person giving the time and
new location when they moved
}
\description{
Return trajectory by person for Simple Trip model
}
