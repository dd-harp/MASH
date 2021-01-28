library(data.table)
library(macro)

check_health_path <- function(health_dt, people_cnt) {
  has_people <- nrow(health_dt) == people_cnt
  has_cols <- all(c("ID", "Start", "Time1", "Level1") %in% colnames(health_dt))
  had_an_event <- health_dt[!is.na(health_dt$Time1),]
  start_set_ok <- all(had_an_event$Level1 != had_an_event$Start)
  c(has_people = has_people, has_cols = has_cols, start_set_ok = start_set_ok)
}

test_that("human_si returns events before first step", {
  parameters <- list(
    recovery_rate = 1 / 200,
    people_cnt = 100,
    duration_days = 10,
    initial_pfpr = 0.3
  )
  current_time <- 0
  human <- human_si_module(parameters)
  health_dt <- human_disease_path(human)
  expect_true(all(check_health_path(health_dt, parameters$people_cnt)))
})


test_that("human_si returns events at each step", {
  parameters <- list(
    recovery_rate = 1 / 200,
    people_cnt = 100,
    duration_days = 10,
    initial_pfpr = 0.3
  )
  current_time <- 0
  human <- human_si_module(parameters)
  health_dt <- human_disease_path(human)
  for (i in 1:10) {
    bites <- macro::forced_si_create_bites(
      parameters$people_cnt, 1/20, current_time, parameters$duration_days)
    bites_dt <- data.table::data.table(
      human = 1:parameters$people_cnt,
      times = bites
    )

    human <- human_si_module(parameters)
    human <- mash_step(human, bites_dt)
    health_dt <- human_disease_path(human)
    expect_true(all(check_health_path(health_dt, parameters$people_cnt)))
    current_time <- current_time + parameters$duration_days
  }
})
