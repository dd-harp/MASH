#' Responsible for generating mosquitoes from the aquatic stage.
#' @export
Aquatic_Poisson <- R6::R6Class(
  classname = "Aquatic_Poisson",
  portable = TRUE,
  cloneable = FALSE,
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(
    #' @description
    #' Construct a set of humans
    #' @param parameters A list of parameters, including `lambda`, a vector of
    #'     emergence rate per site, in mosquitoes per day. Also, `duration` is
    #'     the number of days to run.
    initialize = function(parameters) {
      flog.debug(paste("Aquatic created with parameters ", parameters))
      private$parameters <- parameters
      private$duration <- parameters$duration
      private$lambda <- parameters$lambda
    },

    #' @description
    #' Add a pathogen to this human's pathogens.
    #' @param step_id The integer ID of this step.
    step = function(step_id) {
      out_matrix <- array(integer(private$duration * length(private$lambda)),
                          dim = c(length(private$lambda), private$duration))
      for (day_idx in seq(private$duration)) {
        out_matrix[, day_idx] <- rpois(length(private$lambda), private$lambda)
      }
      private$output <- out_matrix
      invisible(self)
    },

    #' @description
    #' Get the new mosquitoes.
    path = function() {
      private$output
    },

    #' @description
    #' Destructor
    finalize = function() {
      # futile.logger::flog.trace("Human_NULL %i being killed at self: %s , private: %s",private$id,pryr::address(self),pryr::address(private))
    }
  ),

  # private members
  private = list(
    # local fields
    parameters = NULL,
    # A vector of emergence rate per patch, in mosquitoes per day.
    lambda = numeric(1),
    duration = integer(1),
    output = integer(1)
  )
)
