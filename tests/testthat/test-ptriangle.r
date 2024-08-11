# copyright 2018 Rob Carnell

context("test-ptriangle")

test_that("ptriangle", {
  expect_equal(ptriangle(1, 1, 3, 2), 0)
  expect_equal(ptriangle(3, 0, 3, 2), 1)
  expect_equal(ptriangle(-1, -1, 3, 2), 0)
  expect_equal(ptriangle(.5, 0, 1, .5), .5)

  # a <= c <= b at least one strict
  expect_warning(expect_equal(ptriangle(0, 0, 0, 0), NaN))
  expect_warning(expect_equal(ptriangle(.5, 0, 1, 2), NaN))
  expect_warning(expect_equal(ptriangle(.5, 1, 2, .5), NaN))
  expect_warning(expect_equal(ptriangle(c(.5, .6), c(0, 0), c(1, 1),
                                                c(.5, 1.5)), c(.5, NaN)))
})

test_that("ptriangle is consistent with punif", {
  # punif(0.5, 1, 0) is NaN
  # a <= b failed
  expect_warning(expect_equal(ptriangle(.5, 2, 1, 1.5), NaN))
  # punif(c(0, 4, -1, 2), 0, 4) returns 4 numbers
  expect_equal(ptriangle(c(1, 3, -1, .5), c(1, 0, -1, 0), c(3, 3, 3, 1),
                         c(2, 2, 2, .5)), c(0, 1, 0, .5))

  # punif(0.5, 0, NA) is NA
  expect_true(is.na(ptriangle(.5, 0, NA, 3)))
  # punif(0.5, 0, NaN) is NaN
  expect_true(is.nan(ptriangle(.5, 0, 5, NaN)))
  # punif(0.5, 0, Inf) is NaN with a warning
  expect_warning(expect_true(is.nan(ptriangle(.5, 0, Inf, 3))))
})

test_that("ptriangle is not consistent with punif", {
  # punif does recylcing here
  # punif(4, c(1,2), c(2,3,4)) will return 3 numbers
  expect_error(ptriangle(c(1,2,3), c(1,2,3), c(4,5), c(1,2,3)))
})
