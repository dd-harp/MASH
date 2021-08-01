library(R6)
library(futile.logger)

hsi_queue <- function() {
  queue <- list(
    who = integer(0),
    when = integer(0),
    what = integer(0)
  )
  queue
}

hsi_INFECTIOUS <- 1
hsi_RECOVER <- 2

hsi_add_event <- function(queue, who, when, what) {
  queue$who <- c(queue$who, who)
  queue$when <- c(queue$when, when)
  queue$what <- c(queue$what, what)
  queue
}

#' Returns all events that happen at times less-than or equal-to `when`.
#' Removes those events from the queue.
#' When you call this, it returns the modified queue. You have to pull it out
#' of the return value:
#' ```
#' q_who <- hsi_pop_events(queue, 10.0)
#' queue <- q_who["queue"]
#' who <- q_who["chosen"]
#' ```
hsi_pop_events <- function(queue, when) {
  choose <- queue$when <= when
  list(
    chosen = list(
      when = queue$when[choose],
      who = queue$who[choose],
      what = queue$what[choose]
    ),
    queue = list(
      who = queue$who[!choose],
      what = queue$what[!choose],
      when = queue$when[!choose]
    )
  )
}

hsi_length <- function(queue) length(queue$who)

#' Humans with susceptible-infected dynamics
#'
#' A \code{Human_SI} is a module for MASH. It represents a set of humans in
#' the simulation.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * id: integer id
#'  * w: numeric biting weight
#'  * feedingID: id of the \code{\link{Feeding_Resource}} where my biting weight will be added
#'  * siteID: id of the \code{\link{Site}} where my feeding site resides
#'  * tileID: id of the \code{\link{Tile}} where I reside
#'
#' @section **Methods**:
#'  * method: i'm a method!
#'
#' @section **Fields**:
#'  * id: integer id
#'  * w: numeric biting weight
#'  * UNBITTEN: logical flag
#'  * mosquito_id: integer vector of mosquitoes that have bitten me
#'  * mosquito_t: numeric vector of times i was bitten
#'  * bloodFeed: \code{FALSE} corresponds to probing-only events, \code{TRUE} corresponds to combined probe-bloodfeed events.
#'
#' @export
Human_SI <- R6::R6Class(
  classname = "Human_SI",
  portable = TRUE,
  cloneable = FALSE,
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(
    #' @description
    #' Construct a set of humans
    #' @param parameters A list of parameters.
    initialize = function(parameters) {
      flog.debug(paste("Human created with parameters ", parameters))
      private$parameters <- parameters
      private$duration <- parameters$duration
      private$people_cnt <- parameters$people_cnt
      private$initial_pfpr <- parameters$initial_pfpr
      private$recovery_rate <- parameters$recovery_rate
      private$queue <- hsi_queue()
      private$state <- rbinom(
        parameters$people_cnt, 1, parameters$recovery_rate)
      private$day_cnt <- 0
      invisible(self)
    },

    #' @description
    #' Add a pathogen to this human's pathogens.
    #' @param step_id The integer ID of this step.
    step = function(step_id) {
      queue <- private$queue
      recovery_days <- 1 / private$recovery_rate
      for (day_idx in seq(private$duration)) {
        cq <- hsi_pop_events(private$queue, private$day_cnt)
        chosen <- cq["chosen"]
        for (cidx in seq_along(chosen$who)) {
          what <- chosen$what[cidx]
          who <- chosen$who[cidx]
          if (what == hsi_INFECTIOUS) {
            private$state[who] <- 1L
            queue <- hsi_add_event(
              queue, who, day_cnt + recovery_days, hsi_RECOVER)
          } else if (what == hsi_RECOVER) {
            private$state[who] <- 0L
          } else {
            stop(paste("The Human_SI event was unknown: ", what))
          }
          private$day_cnt <- private$day_cnt + 1
        }
        queue <- cq$queue
      }
      private$queue <- queue
      invisible(self)
    },

    #' Infect an individual.
    #' @param who An integer ID for the individual.
    #' @param when_infected A day when the human was infected, possibly
    #'     in the past.
    infect = function(who, when_infected) {
      when <- when_infected + 1L  # Concern with this when we get beyond toy.
      private$queue <- hsi_add_event(private$queue, who, when, hsi_INFECTIOUS)
      invisible(self)
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
    duration = numeric(1),
    day_cnt = integer(0),
    people_cnt = integer(1),
    initial_pfpr = numeric(1),
    recovery_rate = numeric(1),
    queue = NULL,
    state = numeric(0)
  )
)
