#' MASH module that says everybody is in one location.
#'
#' @export
single_location <- function(params) {
  stopifnot("people_cnt" %in% names(params))
  stopifnot(is.numeric(params$people_cnt))
  structure(params, class = "single_location")
}


#' Increment time so that everybody stays in the one location.
#'
#' @param location_module The MASH module.
#' @param health_path The history of health states for each person.
#' @export
mash_step.single_location <- function(location_module, health_path) {
  location_module
}


#' Get the data for where everybody has been. Surprise, it's one place.
#' @param module The single_location module from which to extract results.
#' @return a list of who was at the one place, everybody.
#' @export
location_path.single_location <- function(module) {
  rep(1, module$people_cnt)
}
