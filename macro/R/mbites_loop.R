#' Main loop for a MASH simulation to run with MBITES.
#'
#' @param modules A list of modules, named `location`, `mosquito`,
#'     and `health`.
#'
#' This is responsible for sending and receiving messages among the
#' mosquito, health, and location modules. There is no bloodmeal module in this
#' simulation. Biting takes place inside the mosquito module, but the human
#' module concludes whether a bite could infect a mosquito or has infected a
#' human.
#'
#' In this version of the main loop, the MBITES simulation of mosquitoes plays
#' the part of the bloodmeal and the mosquito module, together. Its interfaces
#' match those of the bloodmeal, so the MBITES mosquito module is a subclass
#' of the Bloodmeal S3 class.
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
step_mbites_mainloop <- function(modules, observer, step_cnt = 1, dump_condition = NULL) {
  location <- modules$location
  health <- modules$health
  mosquito <- modules$mosquito
  if (is.null(dump_condition)) { dump_condition <- function(...) FALSE }

  # These two modules take the past data and yield future data.
  step_idx <- 0
  health_path <- human_disease_path(health)
  observer <- observe_health(observer, health_path, step_idx)
  if (dump_condition("prestep", step_idx)) dump_arguments(
    "prestep", step_idx,
    health=health_path, mosquito=mosquito_trajectory
  )

  duration <- 10
  for (step_idx in 1:step_cnt) {
    step_id <- list(time = (step_idx - 1) * duration, duration = duration)
    observer <- observe_begin_step(observer, step_idx)

    # Location depends on current health.
    location <- mash_step(location, step_id, health_path)
    human_path <- person_path(location)
    observer <- observe_location(observer, human_path, step_idx)

    # Bloodmeal is the central piece where things come together.
    if (dump_condition("bloodmeal", step_idx)) dump_arguments(
      "bloodmeal", step_idx,
      health=health_path, location=human_path
    )
    mosquito <- mash_step(
      mosquito, step_id, health_path, human_path, NULL)
    human_bloodmeal_path <- infects_human_path(mosquito)
    observer <- observe_bloodmeal_human(
      observer, human_bloodmeal_path, step_idx)

    if (dump_condition("postblood", step_idx)) dump_arguments(
      "postblood", step_idx,
      human_meal=human_bloodmeal_path
    )

    # Health moves us to the next step.
    health <- mash_step(health, step_id, human_bloodmeal_path)
    health_path <- human_disease_path(health)
    observer <- observe_health(observer, health_path, step_idx)

    observer <- observe_end_step(observer, step_idx)
  }

  modules$location <- location
  modules$health <- health
  modules$mosquito <- mosquito
  modules$observer <- observer
  modules
}
