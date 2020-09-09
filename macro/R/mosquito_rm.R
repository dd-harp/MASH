#' Make a base set of parameters for R-M mosquitoes.
#'
#' @param patch_cnt The number of patches
#' @param year_days The number of days in a year
#'
#' @export
build_biting_parameters <- function(patch_cnt, year_days = 365) {
  list(
    N = patch_cnt,  # patches
    # emergence matrix
    lambda = matrix(rep(0.1, year_days * patch_cnt), nrow = patch_cnt),
    psi = diag(patch_cnt),  # diffusion matrix
    EIP = rep(12, year_days),
    maxEIP = 12,
    kappa = rep(0.4, patch_cnt),  # biting fraction
    p = 0.9,  # survival
    a = 0.6,   # human biting rate
    year_day_start = 1
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
#' @export
shift_with_open_interval <- function(N) {
  y_shift <- diag(c(numeric(N - 1), 1))
  diag(y_shift[, -1]) <- rep(1, N - 1)
  y_shift
}


#' Turn external parameters into mosquito internal parameters.
#' @param The external parameters.
#' @export
build_internal_parameters <- function(parameters) {
  with(parameters, {
    list(
      N = N,  # patches
      lambda = lambda,  # emergence matrix
      p_psi = p * psi,  # diffusion matrix
      EIP_back = look_back_eip(EIP),
      maxEIP = maxEIP,
      a_kappa = a * kappa,  # biting fraction
      y_shift = shift_with_open_interval(maxEIP + 1),
      year_day_offset = year_day_start - 1
    )
  })
}


#' Create a default state for mosquito-RM model.
#' @param The internal parameter set.
#' @export
build_biting_state <- function(parameters) {
  with(parameters, {
    # Add a category for
    EIP_open <- maxEIP + 1
    list(
      M = rep(0, N),
      Y = matrix(rep(0, N * EIP_open), nrow = N),
      Z = rep(0, N),
      simulation_day = 1
    )
  }
  )
}


#' An aquatic emergence dynamics.
#' @param A vector of emergence rate per patch.
#' @return Returns a vector count of emerged mosquitoes.
#' @export
mosquito_rm_aquatic <- function(lambda) {
  rpois(length(lambda), lambda)
}


#' Dynamics of R-M mosquitoes.
#' @param state The internal state.
#' @param parameters The internal parameters.
#' @param kapp The biting rate from humans.
#' @param aquatic The function to call to get the emergence.
#' @export
mosquito_rm_dynamics <- function(state, parameters, kappa, aquatic) {
  within(state, {
    with(parameters, {
      simulation_day <- simulation_day + 1
      year_day <- day_within_year(simulation_day + year_day_offset)

      M <- p_psi %*% (M + aquatic(lambda[, year_day]))
      Y <- p_psi %*% Y
      Z <- p_psi %*% Z

      broods <- EIP_back[[year_day]]
      if (length(broods) > 0) {
        for (brood in broods) {
          Z = Z + Y[, brood]
        }
      }

      Y0 <- a_kappa * (M - rowSums(Y))
      Y0[Y0 < 0] <- 0
      Y <- Y %*% y_shift
      Y[, 1] <- Y0
    })
  })
}
