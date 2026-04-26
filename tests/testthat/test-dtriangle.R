# Copyright 2026 Rob Carnell

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
  # NaN
  expect_true(is.nan(dunif(0.5, NaN, 1)))
  expect_true(is.nan(dunif(0.5, 0, NaN)))
  expect_true(is.nan(dunif(NaN, 0, 1)))

  expect_true(is.nan(dtriangle(0.5, NaN, 1, 0.5)))
  expect_true(is.nan(dtriangle(0.5, 0, NaN, 0.5)))
  expect_true(is.nan(dtriangle(0.5, 0, 1, NaN)))
  expect_true(is.nan(dtriangle(NaN, 0, 1, 0.5)))

  # NA
  expect_true(is.na(dunif(0.5, NA, 1)))
  expect_true(is.na(dunif(0.5, 0, NA)))
  expect_true(is.na(dunif(NA, 0, 1)))

  expect_true(is.na(dtriangle(0.5, NA, 1, 0.5)))
  expect_true(is.na(dtriangle(0.5, 0, NA, 0.5)))
  expect_true(is.na(dtriangle(0.5, 0, 1, NA)))
  expect_true(is.na(dtriangle(NA, 0, 1, 0.5)))

  # out of order
  expect_warning(expect_true(all(is.nan(dunif(0.5, 1, 0)))))
  # a > c
  expect_warning(expect_true(all(is.nan(dtriangle(0.5, 5, 6, 4)))))
  # b < c
  expect_warning(expect_true(all(is.nan(dtriangle(0.5, 5, 6, 7)))))

  # Inf
  expect_equal(dunif(1, -Inf, 2), 0)
  expect_equal(dunif(1, 0, Inf), 0)
  expect_equal(dunif(1, -Inf, Inf), 0)
  expect_equal(dunif(Inf, 0, 1), 0)

  expect_equal(dtriangle(.5, 0, Inf, 3), 0)
  expect_equal(dtriangle(.5, -Inf, 5, 3), 0)
  expect_equal(dtriangle(.5, 0, Inf, Inf), 0)
  expect_equal(dtriangle(Inf, 0, 1, 0.5), 0)
  expect_equal(dtriangle(-Inf, 0, 1, 0.5), 0)

  # multi-parameters
  expect_equal(dunif(c(0, 4, -1, 2), 0, 4), c(0.25, 0.25, 0, 0.25))
  expect_equal(dtriangle(c(1, 3, -1, .5), c(1, 0, -1, 0), c(3, 3, 3, 1),
                         c(2, 2, 2, .5)), c(0, 0, 0, 2))
})

test_that("dtriangle is not consistent with dunif", {
  # dunif recycles arguments even in non-recyclable lengths
  expect_error(dtriangle(c(1,2,3), c(1,2,3), c(4,5), c(1,2,3)))
})
