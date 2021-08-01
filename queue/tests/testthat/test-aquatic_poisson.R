test_that("aquatic_poisson makes mosquitoes", {
  params <- list(
    lambda = c(30.2, 0.05, 100.7),
    duration = 7
  )
  ap = Aquatic_Poisson$new(params)
  ap$step(9)
  gen <- ap$path()
  expect_equal(dim(gen), c(3, 7))
  expect_gt(sum(gen), 0)
  rs <- rowSums(gen)
  expect_gt(rs[3], rs[1])
  expect_gt(rs[1], rs[2])
})
