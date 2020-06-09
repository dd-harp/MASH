#' Combines given parameters with defaults that have type checks.
#'
#' @param param_definition A dataframe with columns (name, type, default, description).
#'     The default column is a list of values so that they retain their type.
#' @param param_list A list of the settings the user chose.
#'
#' We need data types so that we can pass to C code and save and load
#' parameters. This uses that data, specified in a dataframe, in order to
#' fill out the given parameters with the complete list of defaults.
#'
#' @examples
#' \dontrun{
#' param_definition <- data.frame(
#'   name = c("recovery_rate", "people_cnt", "duration_days", "transmission"),
#'   default = I(c(list(1 / 200), list(100), list(14), list(matrix(c(1,2,3,4), nrow = 2)))),
#'   type = c("numeric", "integer", "numeric", "matrix[numeric]"),
#'   description = c(
#'     "rate per day to recover from malaria infection",
#'     "number of people in population",
#'     "time step in days",
#'     "flow from a to b"
#'   )
#' )
#' parameters <- get_parameters(param_definition, list(people_cnt = 10))
#' }
#' @export
get_parameters <- function(param_definition, param_list) {
  all_params <- vector(mode = "list", length = nrow(param_definition))
  names(all_params) <- param_definition$name
  for (pidx in 1:nrow(param_definition)) {
    all_params[[pidx]] <- with(param_definition[pidx, ], {
      if (name %in% names(param_list)) {
        set_value <- param_list[[name]]
      } else {
        set_value <- default[[1]]
      }
      if (type == "numeric") {
        checked <- as.numeric(set_value)
      } else if (type == "integer") {
        checked <- as.integer(set_value)
      } else if (type == "character") {
        checked <- as.character(set_value)
      } else {
        checked <- set_value
      }
      checked
    })
  }
  all_params
}


show_parameters <- function(param_definition) {
  knitr::kable(param_definition, format = "markdown")
}
