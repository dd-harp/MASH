#' Increments a MASH module by one discrete time step.
#'
#' An S3 method to advance a module by whatever is the simulation
#' time step. We could create a separate time step function for
#' each of the kinds of modules. We could make one for a health time
#' step, one for a location time step, and so on. This would
#' allow us to skip the `...` below and give an actual argument list.
#'
#' @export
mash_step <- function(mash_module, ...) {
  UseMethod("mash_step", mash_module)
}


#' Extracts a time series, by location, of which people are present.
#'
#' An S3 method to read result data from a location module, where
#' that data is indexed first by location and then by people present over time.
#' @seealso \code{\link{person_path}}
#' @export
location_path <- function(location_module) {
  UseMethod("location_path", location_module)
}


#' Extracts a time series, for each person, of their locations over time.
#'
#' An S3 method to read result data from a location module,
#' where that data is indexed by person, making it easy to look up
#' where that person has been over time.
#' @seealso \code{\link{location_path}}
#' @export
person_path <- function(location_module) {
  UseMethod("person_path", location_module)
}


#' Extracts a time series of all bites that can infect a person.
#'
#' An S3 method to read a bloodmeal module and return infectious
#' bites it has predicted.
#' @seealso \code{\link{infects_mosquito_path}}
#' @export
infects_human_path <- function(bloodmeal_module) {
  UseMethod("infects_human_path", bloodmeal_module)
}


#' Extract all bites that are infectious to a mosquito from a bloodmeal module.
#' @seealso \code{\link{infects_human_path}}
#' @export
infects_mosquito_path <- function(bloodmeal_module) {
  UseMethod("infects_mosquito_path", bloodmeal_module)
}


#' Extract the health status of every person from a health module.
#' @export
human_disease_path <- function(human_module) {
  UseMethod("human_disease_path", human_module)
}


#' Main loop for a MASH simulation to run a discrete time step.
#'
#' @param modules A list of modules, named `location`, `bloodmeal`,
#'     and `health`.
#'
#' This function is responsible for calling the modules in the correct
#' order and for passing the results of one module to others.
#' We call the results of a module's time step a path because it is
#' the route that the simulation took over the time step.
#'
#' It assumes that each module implements S3 methods for the
#' \code{\link{mash_step}}, which simulates the next discrete time step.
#' Each module will also implement S3 methods that extract results
#' of the time step so that they can be passed, by this function, to
#' the modules that need them as inputs. There isn't a single function to
#' extract paths because some modules can produce more than one
#' kind of path for other modules to use.
#'
#' @seealso \code{\link{location_path}}, \code{\link{person_path}},
#'     \code{\link{human_disease_path}}, \code{\link{infects_mosquito_path}},
#'     \code{\link{human_infection_path}}
#' @export
step_mainloop <- function(modules, step_cnt = 1) {
  location <- modules$location
  bloodmeal <- modules$bloodmeal
  health <- modules$health

  health_path <- human_disease_path(health)
  for (step_idx in 1:step_cnt) {
    location <- mash_step(location, health_path)
    location_path <- location_path(location)
    bloodmeal <- mash_step(bloodmeal, location_path, health_path)
    bloodmeal_path <- infects_human_path(bloodmeal)
    health <- mash_step(health, bloodmeal_path)
    health_path <- human_disease_path(health)
  }

  list(location = location, bloodmeal = bloodmeal, health = health)
}
