#' macro: Simulation for vector-borne disease.
#'
#' This package is a set of tools for modular simulation.
#'
#' @section The four types of modules:
#'
#' Every simulation has four module types: a human module, mosquito module,
#' location module for human location, and a bloodmeal that simulates bites.
#'
#' @section Available modules:
#'
#' \itemize{
#'   \item `mosquito_rm` Mosquito simulation using a Ross-MacDonald model.
#'   \item `forced_si` Human module.
#'   \item `bloodmeal_linear`
#'   \item `bloodmeal_density`
#'   \item `location_simpletrip`
#'   \item `location_flux`
#' }
#'
#' @docType package
#' @name macro
#' @import data.table
#' @import futile.logger
#' @import Rcpp
#' @useDynLib macro
NULL
