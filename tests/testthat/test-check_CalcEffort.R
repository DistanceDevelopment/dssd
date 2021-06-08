library(dssd)
library(testthat)

context("Check effort calculations")

test_that("Can correctly validate input", {

  expect_error(calculate.effort(L0 = 0, n0 = 0),
               "Values for L0, n0 and all cv.values must be positive.")

  expect_error(calculate.effort(L0 = 10, n0 = 0),
               "Values for L0, n0 and all cv.values must be positive.")

  expect_error(calculate.effort(L0 = 20, n0 = 5, cv.values = seq(0,0.1, length = 10)),
               "Values for L0, n0 and all cv.values must be positive.")

  expect_error(calculate.effort(L0 = 10, n0 = 20, line.point = TRUE),
               "The value of line.point must either be 'line' or 'point'")

})

test_that("Can correctly calculate required effort", {

  results <- calculate.effort(L0 = 10, n0 = 20, line.point = "line",
                              cv.values = seq(0.05, 0.25, length = 20))

  # Check calcs
  by.hand <- (10/20)*(3/0.05^2)
  expect_equal(results$Effort[1], by.hand)
  by.hand <- (10/20)*(3/0.25^2)
  expect_equal(results$Effort[20], by.hand)

  expect_equal(calculate.effort(L0 = 10, n0 = 20, line.point = "line", cv.values = 0.2), data.frame(Effort = 37.5, CV = 0.2))

  expect_equal(calculate.effort(L0 = 10, n0 = 20, line.point = "point", cv.values = 0.2), data.frame(Effort = 37.5, CV = 0.2))

  # Check sorting
  results2 <- calculate.effort(L0 = 10, n0 = 20, line.point = "line",
                              cv.values = seq(0.25, 0.05, length = 20))
  expect_equal(results, results2)

  # Test the calculation
  by.hand <- (20/50)*(3/0.1^2)

  expect_equal(calculate.effort(L0 = 20, n0 = 50, line.point = "point", cv.values = 0.1), data.frame(Effort = by.hand, CV = 0.1))

})
