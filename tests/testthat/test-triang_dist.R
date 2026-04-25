test_that("dtriang handles density and errors", {
  expect_equal(dtriang(0.5, 0, 1, 0.5), 2)
  expect_equal(dtriang(-1, 0, 1, 0.5), 0)
  expect_error(dtriang(0.5, min = 1, max = 0, mode = 0.5))
  expect_error(dtriang(0.5, min = 0, max = 1, mode = 2))
})

test_that("ptriang handles distribution and errors", {
  expect_equal(ptriang(0, 0, 1, 0.5), 0)
  expect_equal(ptriang(1, 0, 1, 0.5), 1)
  expect_equal(ptriang(0.5, 0, 1, 0.5), 0.5)
  expect_error(ptriang(0.5, 1, 0, 0.5))
})

test_that("qtriang handles quantiles and errors", {
  expect_equal(qtriang(0, 0, 1, 0.5), 0)
  expect_equal(qtriang(1, 0, 1, 0.5), 1)
  expect_error(qtriang(-0.1, 0, 1, 0.5))
  expect_error(qtriang(1.1, 0, 1, 0.5))
})

test_that("rtriang produces consistent results", {
  n <- 50
  samples <- rtriang(n, 0, 1, 0.5)
  expect_length(samples, n)
  expect_true(all(samples >= 0 & samples <= 1))
})
