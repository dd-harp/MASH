
#' This bloodmeal is for testing. It sends out the same data each time.
#' @param parameters Configuration for the module. Must have
#'     `duration_days`, `location_cnt`, `human_cnt`.
#' @export
bloodmeal_constant_module <- function(parameters) {
  module <- list(parameters = parameters)
  structure(module, class = "bloodmeal_density")
}


#' Take one time step for a bloodmeal module.
#'
#' @param simulation The blodmeal module.
#' @param health_dt Human health data.
#' @param movement_dt Movement of humans.
#' @param bites_dt Rate of infectious mosquito bites over time in bites per day.
#' @return Returns the simulation that's updated.
#' @export
mash_step.bloodmeal_constant <- function(
  simulation, step_id, health_dt, movement_dt, bites_dt
) {
  NULL
}


#' Extract human bites from the bloodmeal module.
#'
#' @param simulation The bloodmeal_density module.
#' @return A data.table with bite information. These are all bites
#'     of humans where the mosquito was infectious.
#' @export
infects_human_path.bloodmeal_constant <- function(simulation) {
  NULL
}


#' Extract mosquito bites from the bloodmeal module.
#'
#' @param simulation The bloodmeal_density module.
#' @return A data.table with bite information.
#'     These are only bites where the mosquito was not infectious
#'     but the human was.
#' @export
infects_mosquito_path.bloodmeal_constant <- function(simulation) {
  dt <- with(simulation$parameters, {
    data.table::data.table(
      Time = rep(1:duration_days, each = location_cnt),
      Location = c(1:location_cnt, duration_days),
      Bites = rep(20, location_cnt * duration_days)
    )
  })
}
