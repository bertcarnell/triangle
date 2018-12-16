# copyright 2018 Rob Carnell

context("test-ptriangle")

test_that("ptriangle", {
  expect_equal(ptriangle(1, 1, 3, 2), 0)
  expect_equal(ptriangle(3, 0, 3, 2), 1)
  expect_equal(ptriangle(-1, -1, 3, 2), 0)
  expect_equal(ptriangle(.5, 0, 1, .5), .5)
  expect_warning(expect_equal(ptriangle(0, 0, 0, 0), NaN))
  expect_warning(expect_equal(ptriangle(.5, 0, 1, 2), NaN))
  expect_warning(expect_equal(ptriangle(.5, 1, 2, .5), NaN))
  expect_equal(ptriangle(c(1, 3, -1, .5), c(1, 0, -1, 0), c(3, 3, 3, 1),
                               c(2, 2, 2, .5)), c(0, 1, 0, .5))
  expect_warning(expect_equal(ptriangle(c(.5, .6), c(0, 0), c(1, 1),
                                                c(.5, 1.5)), c(.5, NaN)))

})
