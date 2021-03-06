% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bloodmeal_density.R
\name{assign_levels_to_bites}
\alias{assign_levels_to_bites}
\title{Assign bite levels for bites of a single human.}
\usage{
assign_levels_to_bites(health_dt, bite_cnt, day_idx, params)
}
\arguments{
\item{health_dt}{Data for just this human from the health table.}

\item{bite_cnt}{The number of bites to create in day \code{day_idx}.}

\item{params}{Has a \code{day_duration} which is usually 1.}

\item{The}{day within the duration. Day 1 starts at time 0.}
}
\description{
Assign bite levels for bites of a single human.
}
