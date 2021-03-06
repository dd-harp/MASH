% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location_flux.R
\name{location_path.flux}
\alias{location_path.flux}
\title{Return trajectory by location for Flux model}
\usage{
\method{location_path}{flux}(simulation)
}
\arguments{
\item{simulation}{A flux model (most likely made via \code{\link[macro]{flux_module}})}
}
\value{
a list of \code{data.frame}, one for each patch giving the times anyone arrived at or left that patch
}
\description{
Return trajectory by location for Flux model
}
