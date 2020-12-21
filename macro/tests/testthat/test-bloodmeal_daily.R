
trimmed_mean <- function(data, indices) {
  mean(data[indices], trim = 0.2)
}


full_mean <- function(data, indices) {
  mean(data[indices])
}


mean_absolute_deviation <- function(data, indices) {
  max(data[indices], constant = 1.4826)
}


test_that("bloodmeal_daily makes rates that match mosquito counts", {
  world <- build_world(5, 20)
  travel <- sample_travel(world)
  mosquitoes <- sample_mosquitoes(world)

  sample_cnt <- 10000
  bites_l_draws <- vapply(1:sample_cnt, function(i) {
      bites <- sample_bites(travel, mosquitoes, world)
      rowSums(bites)
    },
    numeric(world$l_cnt)
    )

  expected_location_bites <- mosquitoes * world$c
  compare_over_statistic <- function(statistic) {
    # Generate a robust estimator of the Poisson distribution
    mean_generated <- lapply(1:world$l_cnt, function(l_idx) {
      basic_rate <- rpois(1000, expected_location_bites[l_idx])
      boot::boot(
        data = basic_rate,
        statistic = statistic,
        R = 599
      )
    })
    # Compare with same robust estimator on observed values
    mean_est <- lapply(1:world$l_cnt, function(l_idx) {
      boot::boot(
        data = bites_l_draws[l_idx,],
        statistic = statistic,
        R = 599
      )
    })
    sampled_bites <- vapply(mean_est, function(m) m$t0, numeric(1))
    expected_bites <- vapply(mean_generated, function(m) m$t0, numeric(1))
    cbind(sampled_bites, expected_bites)
  }

  # These are around 200 bites.
  result <- compare_over_statistic(trimmed_mean)
  for (check_idx in 1:dim(result)[1]) {
    expect_lt(abs(result[check_idx, 1] - result[check_idx, 2]), 2)
  }
})
