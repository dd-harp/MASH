# Mosquito Ross-Macdonald module
#
# Discrete-time, ten-day timestep built from single-day discrete time.
#
# Refers to Smith, David L., et al. “Ross, Macdonald, and a theory for the
# dynamics and control of mosquito-transmitted pathogens.” PLoS pathog 8.4 (2012): e1002588.
# More direct paper: Reiner Jr, Robert C., et al. "Estimating malaria transmission from
# humans to mosquitoes in a noisy landscape." Journal of the Royal Society Interface
# 12.111 (2015): 20150478.


#' Make a base set of parameters for R-M mosquitoes.
#'
#' @param patch_cnt The number of patches
#' @param year_days The number of days in a year
#'
#' @export
build_biting_parameters <- function(patch_cnt) {
  year_days = 365
  list(
    N = patch_cnt,  # patches
    # emergence matrix
    lambda = matrix(rep(0.1, year_days * patch_cnt), nrow = patch_cnt),
    psi = diag(patch_cnt),  # diffusion matrix
    EIP = rep(12, year_days),  # Extrinsic incubation period,
    maxEIP = 12,  # Maximum length of EIP.
    p = 0.9,  # survival
    # human blood feeding rate, the proportion of mosquitoes that feed on humans each day
    a = 0.6,
    year_day_start = 1,
    # Does not affect calculation value.
    adult_scale = NULL  # Scale for mosquito population.
  )
}


#' Given a day that counts past multiple years, what's year day.
#' @param day integer count of days since start of year.
#' @param day_cnt It's 365 unless you're testing.
#'
#' This is what you get for counting from 1 instead of 0.
#'
#' @export
day_within_year <- function(day, day_cnt = 365) {
  ((day - 1) %% day_cnt) + 1
}


#' This reverses the list of EIP per day of the year.
#'
#' @param EIP a vector of the EIP on a day of the year.
#'
#' It tells you how many days back to get the newly-infected
#' mosquitoes. The value is a ragged array by day of year.
#'
#' @export
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


#' Makes a shift matrix that accumulates the oldest.
#' @param N The size of the square matrix.
#' @return A square matrix. You right-multiply a population, and it gets
#'     a day older by moving a column to the right.
#'
#' This is for half-open intervals, meaning the last column of the matrix
#' doesn't shift. It accumulates individuals that are added from the previous
#' day. This number will be gradually reduced by the survival.
#'
#' @export
shift_with_open_interval <- function(N) {
  y_shift <- diag(c(numeric(N - 1), 1))
  diag(y_shift[, -1]) <- rep(1, N - 1)
  y_shift
}


#' Turn external parameters into mosquito internal parameters.
#' @param parameters The external parameters.
#' @param adult_scale A scale for total mosquito counts for all patches.
#' @export
build_internal_parameters <- function(parameters) {
  with(parameters, {
    adult_scale <- ifelse(is.null(adult_scale), 1.0, adult_scale)
    list(
      N = N,  # patches
      lambda = lambda,  # emergence matrix
      p_psi = p * psi,  # diffusion matrix
      EIP_back = look_back_eip(EIP),
      maxEIP = maxEIP,
      a = a,
      one_day_older = shift_with_open_interval(maxEIP + 1),
      year_day_offset = year_day_start - 1,
      M0 = adult_scale
    )
  })
}


#' Create a default state for mosquito-RM model.
#' @param infectious An array of the count of infectious mosquitoes for each patch.
#' @param replacement The fraction of infectious that should be in incubating for each day.
#'     A good choice for this is the complement of the daily conditional survival.
#' @param maxEIP The maximum length of the EIP for any day of the year, in days.
#' @return A list of the main variables.
#'     \itemize{
#'       \item \eqn{M} is adult females. This will be an array with a float for each patch.
#'       \item \eqn{Y} are incubating mosquitoes, one for each patch, and one for each day
#'         of the EIP, so it's a matrix (patch x EIP day). The first column is newly-hatched.
#'         The last column are those incubating mosquitoes that are beyond the EIP.
#'       \item \eqn{Z} are infectious mosquitoes.
#'       \item \code{simulation_day} This is the count of the number of days into the simulation.
#'     }
#' @export
build_biting_state <- function(infectious, replacement, maxEIP) {
  # Add a category for mosquitoes that are past EIP but not yet
  # removed from the system. This is the last column of the Y matrix.
  N <- length(infectious)
  EIP_open <- maxEIP + 1
  Y_init = matrix(rep(replacement * infectious, EIP_open), nrow = N)
  Y_init[, EIP_open] <- 0  # The last catch-all should be empty.
  list(
    M = rep(1, N),
    Y = Y_init,
    Z = infectious,
    simulation_day = 1
  )
}


#' Make a copy of the state of the Mosqutio-RM model.
#' @param state A list of the state
#' @return A copy of the state. No changes.
#' @export
mosquito_rm_copy_state <- function(state) {
  with(state, {
    list(M = M, Y = Y, Z = Z, simulation_day = simulation_day)
  })
}


#' An aquatic emergence dynamics.
#' @param lambda A vector of emergence rate per patch.
#' @return Returns a vector count of emerged mosquitoes.
#' @export
mosquito_rm_aquatic <- function(lambda) {
  rpois(length(lambda), lambda)
}


#' Dynamics of R-M mosquitoes.
#' @param state The internal state.
#' @param parameters The internal parameters.
#' @param kappa Net infectiousness of humans to mosquitoes, the probability
#'     a mosquito becomes infected after feeding on a human. Vector per patch.
#' @param aquatic The function to call to get the emergence.
#' @export
mosquito_rm_dynamics <- function(state, parameters, kappa, aquatic = mosquito_rm_aquatic) {
  with(parameters, {
    within(state, {
      simulation_day <- simulation_day + 1
      year_day <- day_within_year(simulation_day + year_day_offset)

      # It matters whether we apply survival to the new adults.
      M <- p_psi %*% (M + aquatic(lambda[, year_day]))
      Y <- p_psi %*% Y
      Z <- p_psi %*% Z

      broods <- EIP_back[[year_day]]
      if (length(broods) > 0) {
        for (brood in broods) {
          Z = Z + Y[, brood]
        }
      }

      Y0 <- a * kappa * (M - rowSums(Y))
      Y0[Y0 < 0] <- 0
      Y <- Y %*% one_day_older
      Y[, 1] <- Y0
    })
  })
}


#' Convert the input bloodmeals into an internal input.
#' @param bloodmeal_dt A data table of bloodmeal inputs
#' @return A vector of kappa for the mosquito_rm model.
mosquito_rm_convert_bloodmeal <- function(bloodmeal_dt) {
  numeric(5)
}


#' Create a Mosquito_rm module for Ross-Macdonald mosquitoes
#' @param parameters A block of parameters for this model.
#' @return A new module.
#' @export
mosquito_rm_module <- function(parameters) {
  infectious <- rep(0.4, parameters$N)
  mortality <- 1 - parameters$p
  module <- list(
    external_parameters = parameters,
    parameters = build_internal_parameters(parameters),
    state = build_biting_state(infectious, mortality, parameters$maxEIP)
    )
  class(module) <- "mosquito_rm"
  module
}


mosquito_rm_aggregate_state <- function(output, state) {
  output
}


#' An average of kappa over the past ten days so that we can project forward.
#' @param kappa A matrix of values for each patch, for all days. Nx10.
#' @return A matrix of kappa values for each patch and day, Nx10.
#'
#' We use this function because it can look at trends instead of a
#' simple average. This version weights most recent days to construct
#' a constant value for the future.
#'
#' @export
mosquito_rm_average_kappa <- function(kappa) {
  duration <- dim(kappa)[2]
  weights <- exp((-1 / duration) * (duration - 1:duration))
  weights <- weights / sum(weights)
  one_day <- rowSums(kappa) %*% weights
  as_row <- rep(one_day, duration)
  matrix(as_row, ncol = duration)
}


#' A trend of kappa over the past ten days so that we can project forward.
#' @param kappa A matrix of values for each patch, for all days. Nx10.
#' @return A matrix of kappa values for each patch and day, Nx10.
#'
#' We use this function because it can look at trends instead of a
#' simple average. This version uses an autoregressive process to
#' find a trend.
#'
#' @export
mosquito_rm_trended_kappa <- function(kappa) {
  N <- dim(kappa)[1]
  duration <- dim(kappa)[2]
  # Start with matrix rotated so that it has stride-1 access.
  result <- matrix(nrow = duration, ncol = duration)
  for (patch in 1:N) {
    result[, patch] <- as.vector(predict(
      ar.ols(
        ts(xx, start = 0, end = duration - 1)
        ),
      n.ahead = duration,
      se = FALSE
      ))
  }
  t(duration)
}


#' One discrete time step for the mosquito_rm module.
#' @param module The mosquito_rm module.
#' @param past_kappa Infectiousness to mosquitoes over discrete-time step.
#' @return Observation of the time step and new state as a list
#'     with `future_state` and `output`.
#' @export
mosquito_rm_discrete_step <- function(module, past_kappa) {
  params <- module$parameters
  today_state <- module$state
  for (i in 1:10) {
    today_state <- mosquito_rm_dynamics(today_state, params, past_kappa)
  }
  future_kappa <- mosquito_rm_average_kappa(past_kappa)
  future_state <- mosquito_rm_copy_state(today_state)
  output <- list()
  for (i in 1:10) {
    future_state <- mosquito_rm_dynamics(future_state, params, future_kappa)
    output[[i]] <- future_state$Z
  }
  list(
    future_state = future_state,
    output = do.call(cbind, output)
  )
}


#' One discrete time step for the mosquito_rm module.
#' @param module The mosquito_rm module.
#' @param bloodmeal_dt The input bloodmeal data from the last time step.
#' @return The mosqutio_rm module back again, after the time step.
#' @export
mash_step.mosquito_rm <- function(module, bloodmeal_dt) {
  past_kappa <- mosquito_rm_convert_bloodmeal(bloodmeal_dt)
  step_output <- mosquito_rm_discrete_step(module, past_kappa)
  result <- list(
    external_parameters = module$external_parameters,
    parameters = module$parameters,
    state = step_output$today_state,
    output = step_output$output
  )
  class(result) <- "mosquito_rm"
  result
}
