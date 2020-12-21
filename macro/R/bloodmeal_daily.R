# Assigns bites to people at locations.
# Uses integral counts of bites.
# Assigns bites to individuals with a distribution.
#
# Define the system as having these parameters.
# - h_cnt Number of humans
# - l_cnt Number of locations
# - b Biting weight of each human.
# - c Biting rate of mosquitoes.
# - travel_pattern, which is multinomial for where humans go.
#
# and this state.
# - tar matrix of locations x humans, time at each location.
# - mosquitoes, which is number of mosquitoes at each location.
#
# Then draw from that.

bloodmeal_nbinom <- function(eir, dispersion) {
  if (eir > 0) {
    p <- dispersion / (dispersion + eir)
    k <- (eir * p) / (1 - p)
    rnbinom(1, size = k, prob = p)
  } else {
    0
  }
}


#' Create parameters for a small model of mosquito bites.
#' @param locations The count of locations.
#' @param humans_per_location The count of humans per location, a single integer.
#' @export
build_world <- function(locations, humans_per_location) {
  l_cnt <- as.integer(locations)
  humans_per_location <- as.integer(humans_per_location)
  h_cnt <- humans_per_location * l_cnt
  home <- rep(1:l_cnt, each = humans_per_location)
  b <- runif(h_cnt, 0.5, 1)
  c <- runif(l_cnt, 0.2, 0.5)
  # A human in a location has a distribution for where they go.
  travel_distribution <- function (l_idx, l_cnt) {
    shape = 1
    preference <- rexp(l_cnt, shape)
    highest <- which.max(preference)
    save <- preference[l_idx]
    preference[l_idx] <- highest
    preference[highest] <- save
    preference / sum(preference)
  }
  location_travel <- vapply(
    1:l_cnt,
    function(l_idx) travel_distribution(l_idx, l_cnt), FUN.VALUE = numeric(l_cnt)
  )
  stopifnot(abs(colSums(location_travel) - rep(1, l_cnt)) < 1e-10)
  # Each human's travel is a draw from that distribution, for 30min segments.
  travel_pattern <- function(h_idx) {
    time_chunks <- rmultinom(1, 48, location_travel[, home[h_idx]])
    time_chunks / sum(time_chunks)
  }
  tar <- vapply(1:h_cnt, travel_pattern, numeric(l_cnt))
  stopifnot(abs(colSums(tar) - rep(1, l_cnt)) < 1e-10)
  stopifnot(dim(tar) == c(l_cnt, h_cnt))
  list(
    location_cnt = l_cnt,
    human_cnt = h_cnt,
    b = b,
    c = c,
    dispersion = 1.5,
    mosquito_cnt = 1000,
    travel_pattern = travel_pattern
  )
}


#' Given a probability to be in each place, sample human movement.
#' @param world Parameters that include a travel pattern matrix.
#' @return A matrix of dwell locations for each person.
#' @export
sample_travel <- function(world) {
  with(world, {
    tar <- vapply(1:human_cnt, travel_pattern, numeric(location_cnt))
    stopifnot(abs(colSums(tar) - rep(1, location_cnt)) < 1e-10)
    stopifnot(dim(tar) == c(location_cnt, human_cnt))
    tar
  })
}


#' Creates a population of mosquitoes for a given day.
#' @param world The parameters for the simulate
#' @export
sample_mosquitoes <- function(world) {
  with(world, {
    multitudes_of_mosquitoes <- rbinom(location_cnt, 5, 0.4)
    mosquitoes <- rbinom(location_cnt, mosquito_cnt*multitudes_of_mosquitoes, 0.2)
    mosquitoes
  })
}


#' Determines an integral numbers of bites at each location.
#' @param travel is the location x human matrix of dwell times for each human.
#' @param mosquitoes The array of mosquito counts for this day.
#' @param bite_rate The biting rate for each location. It's `a` in our notation.
#' @param bite_weight Bite weight per person.
#' @param world is parameters for the simulation.
#' @return a location x human matrix of bites.
#' @export
sample_bites <- function(travel, mosquitoes, biting_rate, bite_weight, world) {
  # Naming: It's what.axes, so bites = count, bite_rate = numeric,
  # bite_weight=numeric. Axes are l=location, h=human, or lh for matrix.
  with(world, {
    bite_weight.lh <- travel %*% diag(bite_weight)
    bite_weight.l <- rowSums(bite_weight.lh)
    bite_norm.l <- ifelse(bite_weight.l > 0, 1 / bite_weight.l, 0)

    bite_rate.l <- mosquitoes * biting_rate
    fraction_of_bites_to_each_human <- diag(bite_norm.l) %*% bite_weight.lh
    bite_rate.h <- bite_rate.l %*% fraction_of_bites_to_each_human
    bite_rate.lh <- fraction_of_bites_to_each_human * bite_rate.l

    logdebug(paste("sample_bites rate", paste0(bite_rate.h, collapse = ", ")))
    bites.h <- vapply(
      bite_rate.h,
      function(bite_rate) {
       bloodmeal_nbinom(bite_rate, dispersion)
      },
      numeric(1)
    )
    bites.lh <- vapply(
      1:human_cnt,
      function(h_idx) {
        bite_cnt <- bites.h[h_idx]
        if (bite_cnt > 0) {
          rmultinom(1, bite_cnt, bite_rate.lh[, h_idx])
        } else {
          numeric(location_cnt)
        }
        },
      numeric(location_cnt)
    )
    bites.lh
  })
}

