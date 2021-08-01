mrm_check_parameters <- function(parameters) {
  required <- c(
    "duration", "N", "lambda", "psi", "biting_weight", "EIP", "maxEIP",
    "p", "a", "infected_fraction", "year_day_start"
  )
  if (!(setequal(required, names(parameters)))) {
    inreq <- paste0(setdiff(required, names(parameters)), collapse = ", ")
    inpar <- paste0(setdiff(names(parameters), required), collapse = ", ")
    stop(paste(
      "mosquito_rm_module parameters missing", inreq,
      "and won't use given parameters:", inpar
    ))
  }
  for (pv_idx in seq(parameters)) {
    pv <- parameters[[pv_idx]]
    if (is.null(pv) || is.na(pv)) {
      logerror(paste("parameter", names(parameters)[pv_idx], "is null or na"))
    }
  }
}

#' Make a base set of parameters for R-M mosquitoes.
#'
#' @param patch_cnt The number of patches
#' @param year_days The number of days in a year
#'
#' @export
build_biting_parameters <- function(patch_cnt) {
  year_days = 365L
  maxEIP = 12L
  list(
    duration = 10L,  # number of days in module time step
    N = patch_cnt,  # patches
    # emergence matrix, patches x days of the year.
    # mosquito count is \lambda * p / (1-p) where p is
    # daily survival.
    lambda = matrix(rep(100, year_days * patch_cnt), nrow = patch_cnt),
    psi = diag(patch_cnt),  # diffusion matrix
    maxEIP = maxEIP,  # Maximum length of EIP.
    # EIP changes through the year.
    EIP = rep(maxEIP, year_days),  # Extrinsic incubation period,
    p = 0.9,  # conditional daily survival
    # human blood feeding rate, the proportion of mosquitoes that feed
    # on humans each day
    a = 0.6,
    # Fraction of mosquitoes infected on any day.
    infected_fraction = rep(0.1, patch_cnt),
    biting_weight = 0.5,
    # Day 1 of the simulation is this day of the year.
    year_day_start = 1
  )
}


day_within_year <- function(day, day_cnt = 365L) {
  ((day - 1L) %% day_cnt) + 1L
}


#' This takes EIP as days in the future and returns it as days into the past.
#'
#' @param EIP a vector of the number of days from infection to infectiousness
#'     on each day of the year. 365 long.
#' @return A list, 365 days long, where each entry is the days from
#'     which to get the newly-adult mosquitoes.
#'
#' EIP is extrinsic incubation period.
#' It tells you how many days back to get the newly-infected
#' mosquitoes. It could be zero or more than one previous day.
#' The value is a ragged array by day of year.
look_back_eip <- function(EIP) {
  day_cnt <- length(EIP)
  look <- lapply(1:day_cnt, function(x) numeric(0))
  for (bite_day in 1:day_cnt) {
    incubate_day <- day_within_year(bite_day + EIP[bite_day], day_cnt)
    backwards <- look[[incubate_day]]
    look[[incubate_day]] <- c(backwards, EIP[bite_day])
  }
  look
}


#' Makes a shift matrix that accumulates the oldest age group.
#'
#' @param N The integer size of the square matrix.
#' @return A square matrix. You right-multiply a population, and it gets
#'     a day older by moving a column to the right.
#'
#' We use this matrix to make mosquitoes one day older. It's for the Y
#' mosquitoes, the ones that are infected.
#'
#' Takes this matrix:
#'  1 2 3
#'  6 7 8
#'
#' and turns it into this matrix:
#'  0 1 5
#'  0 6 15
#'
#' This is for half-open intervals, meaning the last column of the matrix
#' doesn't shift. It accumulates individuals that are added from the previous
#' day. This number will be gradually reduced by the survival.
shift_with_open_interval <- function(N) {
  y_shift <- diag(c(numeric(N - 1), 1))
  diag(y_shift[, -1]) <- rep(1, N - 1L)
  y_shift
}


#' Given parameters from a user, calculates parameters easier to compute.
#'
#' @param external_parameters The parameters as the user specified them.
#' @param adult_scale A scale for total mosquito counts for all patches.
#' @returns A list of parameters computed from the external ones so that they
#'     are easier to use during calculation of time steps.
build_internal_parameters <- function(external_parameters) {
  with(external_parameters, {
    list(
      duration = duration,  # number of days in module time step
      N = N,  # patches
      lambda = lambda,  # emergence matrix
      p_psi = p * psi,  # diffusion matrix with survival included.
      EIP_back = look_back_eip(EIP),
      maxEIP = maxEIP,
      a = a,
      one_day_older = shift_with_open_interval(maxEIP + 1),
      year_day_start = year_day_start
    )
  })
}


#' Convert the input bloodmeals into an internal input.
#'
#' @param bloodmeal_dt A data table of bloodmeal inputs
#' @return A matrix of kappa for the mosquito_rm model.
#'     Dimensions are N x duration of the time step.
mosquito_rm_convert_bloodmeal <- function(bloodmeal_dt) {
  places <- sort(unique(bloodmeal_dt$Location))
  matrix(bloodmeal_dt[order(Time, Location)]$Bites, nrow = length(places))
}


#' Dynamics of Ross-Macdonald mosquitoes.
#'
#' @param state The internal state.
#' @param parameters The internal parameters.
#' @param bites Net infectiousness of humans to mosquitoes expressed
#'     as a number of infectious bites in each location. The units on bites
#'     must match the M units, which can be either a fraction or a count.
#' @param aquatic The function to call to get the emergence.
#' @return a list with five values: the vectors `M`, `Y`, `Z`, and the
#'     parameters `a` and `simulation_day`.
#'
#' This is a single-day dynamics step for a discrete-time analog
#' of the Ross-Macdonald model. The aquatic lifecycle is stochastic, but
#' the rest is deterministic.
mosquito_rm_dynamics <- function(
  state, parameters, bites, aquatic = mosquito_rm_aquatic
) {
  with(parameters, {
    with(state, {
      simulation_day <- simulation_day + 1
      year_day <- day_within_year(simulation_day + year_day_start - 1)

      # It matters whether we apply survival to the new adults.
      M <- p_psi %*% (M + aquatic(lambda[, year_day]))
      Y <- p_psi %*% Y
      Z <- p_psi %*% Z

      broods <- EIP_back[[year_day]]
      # There could be 0 or >1 broods due to EIP changes over season.
      if (length(broods) > 0) {
        for (brood in broods) {
          Z = Z + Y[, brood]
          Y[, brood] <- 0
        }
      }

      # This would be a * kappa * (M - rowSums(Y)) but has been calculated
      # inside the bloodmeal because we sent it M, Y, Z, and a.
      Y0 <- min(bites, (M - rowSums(Y) - Z))
      Y0[Y0 < 0] <- 0
      Y <- Y %*% one_day_older
      Y[, 1] <- Y0
      list(
        M = M,
        Y = Y,
        Z = Z,
        simulation_day = simulation_day
      )
    })
  })
}


#' Create a default state for mosquito-RM model.
#'
#' @param parameters The initial array of parameters.
#' @return A list of the main variables.
#'     \itemize{
#'       \item \eqn{M} is adult females. This will be an array with a float
#'           for each patch.
#'       \item \eqn{Y} are incubating mosquitoes, one for each patch, and one
#'         for each day of the EIP, so it's a matrix (patch x EIP day). The
#'         first column is newly-hatched. The last column are those incubating
#'         mosquitoes that are beyond the EIP.
#'       \item \eqn{Z} are infectious mosquitoes.
#'       \item \code{simulation_day} This is the count of the number of days
#'           into the simulation.
#'     }
mosquito_rm_build_biting_state <- function(parameters) {
  pnames <- c(
    "infected_fraction", "p", "N", "EIP", "lambda", "maxEIP"
  )
  stopifnot(all(pnames %in% names(parameters)))
  f <- parameters$infected_fraction
  p <- parameters$p
  b <- (1 - p) * f / (1 + f)  # approximate biting rate.
  for (solve_location_idx in 1:parameters$N) {
    location_func <- make_tosolve(parameters, solve_location_idx)
    b[solve_location_idx] = optim(
      b[solve_location_idx],
      location_func,
      method = "Brent", lower = 0, upper = 1)$par
  }
  M <- parameters$lambda[, parameters$year_day_start] * p / (1 - p)
  EIP_open <- parameters$maxEIP + 1
  EIP <- parameters$EIP[parameters$year_day_start]
  Y <- matrix(0, nrow = parameters$N, ncol = EIP_open)
  Z <- M[]
  for (init_idx in 1:parameters$N) {
    x <- long_term(b[init_idx], parameters, init_idx)
    # The long-term estimate is a total Y. We allocate that into
    # successive waves
    Y[init_idx, 1:EIP] <- x[2:(2 + EIP - 1)]
    Z[init_idx] <- x[length(x)]  # And Z holds Z.
  }
  list(
    M = M,
    Y = Y,
    Z = Z,
    simulation_day = 1
  )
}

#' Make a copy of the state of the Mosquito-RM model.
#'
#' @param state A list of the state
#' @return A copy of the state with no changes.
mosquito_rm_copy_state <- function(state) {
  state_cp <- with(state, {
    list(M = M, Y = Y, Z = Z, simulation_day = simulation_day)
  })
  same_names <- setequal(names(state_cp), names(state))
  if (!same_names) {
    stop(c(paste(names(state_cp), collapse = ", "),
           paste(names(state), collapse = ", ")))
  }
  state_cp
}


Mosquito_RM <- R6::R6Class(
  classname = "Mosquito_RM",
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
      mrm_check_parameters(parameters)
      private$external_parameters <- parameters
      private$state = mosquito_rm_build_biting_state(parameters)
      private$parameters <- build_internal_parameters(parameters)

      day_cnt <- parameters$duration
      putative_past <- data.table::data.table(
        Location = rep(1:parameters$N, day_cnt),
        Time = rep(0:(parameters$duration - 1), each = parameters$N),
        M = rep(state$M, day_cnt),
        Y = rep(rowSums(state$Y), day_cnt),
        Z = rep(state$Z, day_cnt)
      )
      putative_past[, c("a") := parameters$a]
      private$output <- putative_past
    },

    #' @description
    #' Add a pathogen to this human's pathogens.
    #' @param step_id The integer ID of this step.
    #' @param bloodmeal_dt The input bloodmeal data from the last time step.
    #'     This has Bites, Time, Location in a data table.
    step = function(step_id, bloodmeal_dt) {
      # Let's consider how to get the bloodmeal information into this module.
      past_bites <- mosquito_rm_convert_bloodmeal(bloodmeal_dt)
      # mosquito_rm_discrete_step
      params <- private$parameters
      today_state <- private$state
      for (i in 1:params$duration) {
        today_state <- mosquito_rm_dynamics(today_state, params, bites_arr[, i])
      }
      future_kappa <- mosquito_rm_average_kappa(bites_arr)
      future_state <- mosquito_rm_copy_state(today_state)
      output <- vector(mode = "list", length = params$duration)
      for (i in 1:params$duration) {
        future_state <- mosquito_rm_dynamics(
          future_state, params, future_kappa[, i])
        output[[i]] <- with(future_state, data.table(
          Location = 1:length(M),
          Time = simulation_day - 1,
          M = M,
          Y = rowSums(Y),
          Z = Z
        ))
      }
      unified_out <- data.table::rbindlist(output)
      unified_out[, c("a") := params$a]
      colnames(unified_out) <- c("Location", "Time", "M", "Y", "Z", "a")
      private$state <- today_state
      private$output <- unified_out
      invisible(self)
    },

    #' @description
    #' Destructor
    finalize = function() {
      # futile.logger::flog.trace("Human_NULL %i being killed at self: %s , private: %s",private$id,pryr::address(self),pryr::address(private))
    }
  ),

  private = list(
    # local fields
    external_parameters = list(),
    parameters = list(),
    state = list(),
    output = NULL
  )
)
