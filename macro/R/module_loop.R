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


#' Extracts a time series of all bites that can infect a person.
#'
#' An S3 method to read a bloodmeal module and return infectious
#' bites it has predicted.
#' @seealso \code{\link{infects_mosquito_path}}
#' @export
infects_mosquito_path <- function(bloodmeal_module) {
  UseMethod("infects_mosquito_path", bloodmeal_module)
}


#' Extract all bites that are infectious to a mosquito from a bloodmeal module.
#' @seealso \code{\link{infects_human_path}}
#' @export
mosquito_path <- function(bloodmeal_module) {
  UseMethod("mosquito_path", bloodmeal_module)
}


#' Extract the health status of every person from a health module.
#' @param human_module The module for human health.
#' @export
human_disease_path <- function(human_module) {
  UseMethod("human_disease_path", human_module)
}


dump_arguments <- function(fn, step_idx, ...) {
  datasets <- list(...)
  for (didx in seq(datasets)) {
    datafn <- paste0(
      fn, step_idx, "_", names(datasets)[didx], ".csv", collapse="")
    dt <- datasets[[didx]]
    logdebug(paste("writing to", datafn, "a", paste0(class(dt), collapse=", ")))
    data.table::fwrite(dt, datafn)
  }
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
step_mainloop <- function(modules, observer, step_cnt = 1, dump_condition = NULL) {
  location <- modules$location
  bloodmeal <- modules$bloodmeal
  health <- modules$health
  mosquito <- modules$mosquito
  if (is.null(dump_condition)) { dump_condition <- function(...) FALSE }

  # These two modules take the past data and yield future data.
  step_idx <- 0
  mosquito_trajectory <- mosquito_path(mosquito)
  observer <- observe_mosquito(observer, mosquito_trajectory, step_idx)
  health_path <- human_disease_path(health)
  observer <- observe_health(observer, health_path, step_idx)

  for (step_idx in 1:step_cnt) {
    observer <- observe_begin_step(observer, step_idx)

    # Location depends on current health.
    location <- mash_step(location, health_path)
    human_path <- person_path(location)
    observer <- observe_location(observer, human_path, step_idx)

    # Bloodmeal is the central piece where things come together.
    if (dump_condition("bloodmeal", step_idx)) dump_arguments(
      "bloodmeal", step_idx,
      health=health_path, location=human_path, mosquito=mosquito_trajectory
    )
    bloodmeal <- mash_step(bloodmeal, health_path, human_path, mosquito_trajectory)
    human_bloodmeal_path <- infects_human_path(bloodmeal)
    observer <- observe_bloodmeal_human(observer, human_bloodmeal_path, step_idx)
    mosquito_bloodmeal_path <- infects_mosquito_path(bloodmeal)
    observer <- observe_bloodmeal_mosquito(observer, mosquito_bloodmeal_path, step_idx)

    # The mosquito moves us to the next step.
    mosquito <- mash_step(mosquito, mosquito_bloodmeal_path)
    mosquito_trajectory <- mosquito_path(mosquito)
    observer <- observe_mosquito(observer, mosquito_trajectory, step_idx)

    # Health moves us to the next step.
    health <- mash_step(health, human_bloodmeal_path)
    health_path <- human_disease_path(health)
    observer <- observe_health(observer, health_path, step_idx)

    observer <- observe_end_step(observer, step_idx)
  }

  modules$location <- location
  modules$bloodmeal <- bloodmeal
  modules$health <- health
  modules$mosquito <- mosquito
  modules$observer <- observer
  modules
}
