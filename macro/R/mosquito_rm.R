build_biting_parameters <- function(patch_cnt, year = 365) {
  list(
    N = patch_cnt,  # patches
    lambda = matrix(rep(0.1, patch_cnt, year), nrow = year),  # emergence matrix
    psi = diag(patch_cnt),  # diffusion matrix
    EIP = rep(12, year),
    maxEIP = 12,
    kappa = rep(0.4, patch_cnt),  # biting fraction
    p = 0.9,  # survival
    a = 0.6,   # human biting rate
    year_day_start = 1
  )
}


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


#' This reverses the list of EIP per day of the year.
#'
#' It tells you how many days back to get the newly-infected
#' mosquitoes. The value is a ragged array by day of year.
look_back_eip <- function(EIP) {
  day_cnt <- length(EIP)
  look <- lapply(1:day_cnt, function(x) numeric(0))
  for (bite_day in 1:day_cnt) {
    incubate_day <- (bite_day + EIP[bite_day]) %% day_cnt
    incubate_day <- ifelse(incubate_day, incubate_day, day_cnt)
    backwards <- look[[incubate_day]]
    look[[incubate_day]] <- c(backwards, EIP[bite_day])
  }
  look
}


test_look_back_eip <- function() {
  ll <- look_back_eip(c(3,3,2,2,2))
  compare <- list(c(2), c(2), c(0), c(3), c(3, 2))
  for (i in 1:5) {
    stopifnot(ll[[i]] == compare[[i]])
  }
}


build_internal_parameters <- function(parameters) {
  with(parameters, {
    y_shift <- c(numeric(maxEIP - 1), 1)
    y_shift <- diag(y_shift[1, ]) <- rep(1, maxEIP - 1)
    list(
      N = N,  # patches
      lambda = lambda,  # emergence matrix
      p_psi = p * psi,  # diffusion matrix
      EIP_back = test_look_back_eip(EIP),
      a_kappa = a * kappa,  # biting fraction
      y_shift = y_shift,
    )
  })
}


mosquito_rm_aquatic <- function(lambda) {
  rpois(length(lambda), lambda)
}


mosquito_rm_dynamics <- function(state, parameters, kappa, aquatic) {
  within(c(state, parameters), {
    simulation_day <- simulation_day + 1
    year_day <- simulation_day - year_day_start + 1

    M <- p_psi %*% (M + aquatic(lambda[, year_day]))
    Y <- p_psi %*% Y
    Z <- p_psi %*% Z

    broods <- EIP_back[[year_day]]
    if (length(broods) > 0) {
      for (brood in broods) {
        Z = Z + Y[, brood]
      }
    }

    Y0 <- a_kappa * (M - colSums(Y))
    Y0[Y0 < 0] <- 0
    Y <- Y %*% y_shift
    Y[, 1] <- Y0
  })
}
