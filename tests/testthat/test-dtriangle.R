context("test-dtriangle")

test_that("dtriangle", {
  expect_equal(dtriangle(1, 1, 3, 2), 0) # x < a
  expect_equal(dtriangle(3, 0, 3, 2), 0) # x > b
  expect_equal(dtriangle(-1, -1, 3, 2), 0) # x = a
  expect_equal(dtriangle(.5, 0, 1, .5), 2) # = 2 (x - a) / (b-a) / (c-a)

  expect_warning(expect_equal(dtriangle(0, 0, 0, 0), NaN))
  expect_warning(expect_equal(dtriangle(.5, 0, 1, 2), NaN))
  expect_warning(expect_equal(dtriangle(.5, 1, 2, .5), NaN))
  expect_equal(dtriangle(c(1, 3, -1, .5), c(1, 0, -1, 0), c(3, 3, 3, 1),
                               c(2, 2, 2, .5)), c(0, 0, 0, 2))
  expect_warning(expect_equal(dtriangle(c(.5, .6), c(0, 0), c(1, 1),
                                         c(.5, 1.5)), c(2, NaN)))
  # regression test
  expect_true(all(!is.nan(dtriangle(0:10, a = 2, b = 5, c = 2))))
  expect_warning(expect_true(is.nan(dtriangle(.5, 1, 1, 1))))

  # more case testing
  expect_equal(dtriangle(0, 1, 3, 2), 0)
  expect_equal(dtriangle(1, 1, 3, 2), 0)
  expect_equal(dtriangle(1.5, 1, 3, 2), 0.5)
  expect_equal(dtriangle(2, 1, 3, 2), 1)
  expect_equal(dtriangle(2.5, 1, 3, 2), 0.5)
  expect_equal(dtriangle(3, 1, 3, 2), 0)
  expect_equal(dtriangle(4, 1, 3, 2), 0)

  expect_equal(dtriangle(0, 1, 3, 1), 0)
  expect_equal(dtriangle(1, 1, 3, 1), 1)
  expect_equal(dtriangle(2, 1, 3, 1), 0.5)
  expect_equal(dtriangle(3, 1, 3, 1), 0)
  expect_equal(dtriangle(4, 1, 3, 1), 0)

  expect_equal(dtriangle(0, 1, 3, 3), 0)
  expect_equal(dtriangle(1, 1, 3, 3), 0)
  expect_equal(dtriangle(2, 1, 3, 3), 0.5)
  expect_equal(dtriangle(3, 1, 3, 3), 1)
  expect_equal(dtriangle(4, 1, 3, 3), 0)
})

test_that("dtriangle is consistent with dunif", {
  # dunif(0.5, 1, 0) is NaN with a warning
  expect_warning(expect_true(is.nan(dtriangle(.5, 0, -1, 0))))
  # dunif(0.5, 0, NA) is NA
  expect_true(is.na(dtriangle(.5, 0, NA, 3)))
  # dunif(0.5, 0, NaN) is NaN
  expect_true(is.nan(dtriangle(.5, 0, 5, NaN)))
  expect_true(is.nan(dtriangle(.5, 0, NaN, 1)))
  expect_true(is.nan(dtriangle(.5, NaN, 5, 1)))
  # dunif(0.5, 0, Inf) is 0
  # dunif(0.5, -Inf, 1) is 0
  expect_equal(dtriangle(.5, 0, Inf, 3), 0)
  expect_equal(dtriangle(.5, -Inf, 5, 3), 0)
  expect_equal(dtriangle(.5, 0, Inf, Inf), 0)
  # dunif(NA, 0, 1) is NA
  expect_true(is.na(dtriangle(NA, 0, 1, 0.5)))
  # dunif(NaN, 0, 1) is NaN
  expect_true(is.na(dtriangle(NaN, 0, 1, 0.5)))
  # dunif(Inf, 0, 1) is 0
  expect_equal(dtriangle(Inf, 0, 1, 0.5), 0)
  expect_equal(dtriangle(-Inf, 0, 1, 0.5), 0)
})

test_that("dtriangle is not consistent with dunif", {
  # dunif recycles arguments even in non-recylcable lengths
  expect_error(dtriangle(c(1,2,3), c(1,2,3), c(4,5), c(1,2,3)))
})
