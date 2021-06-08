#' MASH module that says everybody is in one location.
#'
#' @export
single_location <- function(params) {
  stopifnot(all(c("home", "duration_days") %in% names(params)))
  simulation <- list(
    params = params,
    home = params$home,
    time = 0.0
  )
  structure(simulation, class = "single_location")
}


#' Increment time so that everybody stays in the one location.
#'
#' @param location_module The MASH module.
#' @param health_path The history of health states, which is ignored here.
#' @export
mash_step.single_location <- function(location_module, step_id, health_path) {
  # Nothing to do besides updating time because nobody moves.
  location_module$time <- location_module$time +
      location_module$params$duration_days
  structure(location_module, class = "single_location")
}

location_path.single_location <- function(simulation) {
  NULL
}

#' Retrieve list of where people have gone.
#'
#' @param simulaton A module of class single_location.
#' @export
person_path.single_location <- function(simulation) {
  data.table::data.table(
    ID = 1:length(simulation$home),
    Location = simulation$home,
    Time = rep(simulation$time, length(simulation$home))
  )
}
