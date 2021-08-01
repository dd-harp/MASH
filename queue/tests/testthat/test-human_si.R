test_that("create a human", {
  params <- list(
    duration = 10,
    people_cnt = 20L,
    initial_pfpr = 0.4,
    recovery_rate = 0.01
  )
  humans <- Human_SI$new(params)
  humans$step()
  expect_equal(2 * 2, 4)
})


test_that("hsi_queue can add events", {
  q <- queue:::hsi_queue()
  q <- queue:::hsi_add_event(q, 3L, 28.0, hsi_INFECTIOUS)
  q <- queue:::hsi_add_event(q, 5L, 20.0, hsi_INFECTIOUS)
  q <- queue:::hsi_add_event(q, 3L, 38.0, hsi_RECOVER)
  expect_equal(queue::hsi_length(q), 3L)
})


test_that("hsi_queue can pop events with no events", {
  q <- queue:::hsi_queue()
  q <- queue:::hsi_add_event(q, 3L, 28.0, hsi_INFECTIOUS)
  q <- queue:::hsi_add_event(q, 5L, 20.0, hsi_INFECTIOUS)
  q <- queue:::hsi_add_event(q, 3L, 38.0, hsi_RECOVER)
  qe <- queue:::hsi_pop_events(q, 10)
  q <- qe$queue
  e <- qe$chosen
  expect_equal(length(e$who), 0L)
})


test_that("hsi_queue can pop events with some events", {
  q <- queue:::hsi_queue()
  q <- queue:::hsi_add_event(q, 3L, 28.0, hsi_INFECTIOUS)
  q <- queue:::hsi_add_event(q, 5L, 20.0, hsi_INFECTIOUS)
  q <- queue:::hsi_add_event(q, 3L, 38.0, hsi_RECOVER)
  qe <- queue:::hsi_pop_events(q, 30.0)
  q <- qe$queue
  e <- qe$chosen
  expect_equal(length(e$who), 2L)
  expect_equal(queue:::hsi_length(q), 1L)
})
