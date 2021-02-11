library(data.table)


all_movement_locations <- function(location_dt) {
  start_column <- 2
  location_columns <- c(start_column, grep("Location", colnames(location_dt)))
  locations <- unique(as.vector(as.matrix(location_dt[, ..location_columns])))
  locations[is.finite(locations)]
}


test_that("bloodmeal processes sample data", {
  human_cnt <- 10L
  place_cnt <- 5L
  bite_cnt <- 20L
  time_step <- 10.0
  human_events <- macro:::sample_humans_at_location()
  bites_dt <- macro:::sample_mosquito_kappa()
  result <- macro:::bld_bites_at_location(human_events, bites_dt)
})
