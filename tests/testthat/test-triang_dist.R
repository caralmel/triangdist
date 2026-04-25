
test_that("dtriang calcula la densidad correctamente", {
  expect_equal(dtriang(0.5, min = 0, max = 1, mode = 0.5), 2)
  expect_equal(dtriang(-1, min = 0, max = 1, mode = 0.5), 0)
  expect_equal(dtriang(2, min = 0, max = 1, mode = 0.5), 0)
})

test_that("ptriang calcula la probabilidad acumulada", {
  expect_equal(ptriang(1, min = 0, max = 1, mode = 0.5), 1)
  expect_equal(ptriang(0, min = 0, max = 1, mode = 0.5), 0)
})

test_that("Errores y validaciones funcionan", {
  expect_error(dtriang(0.5, min = 10, max = 5, mode = 7))
  expect_error(dtriang(0.5, min = 0, max = 1, mode = 1.5))
})

test_that("rtriang genera valores consistentes", {
  n <- 100
  samples <- rtriang(n, min = 0, max = 1, mode = 0.5)
  expect_length(samples, n)
  expect_true(all(samples >= 0 & samples <= 1))
})
