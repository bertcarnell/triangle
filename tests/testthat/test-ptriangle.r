# Copyright 2026 Rob Carnell

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
  # NaN
  expect_true(is.nan(punif(0.5, NaN, 1)))
  expect_true(is.nan(punif(0.5, 0, NaN)))
  expect_true(is.nan(punif(NaN, 0, 1)))

  expect_true(is.nan(ptriangle(0.5, NaN, 1, 0.5)))
  expect_true(is.nan(ptriangle(0.5, 0, NaN, 0.5)))
  expect_true(is.nan(ptriangle(0.5, 0, 1, NaN)))
  expect_true(is.nan(ptriangle(NaN, 0, 1, 0.5)))

  # NA
  expect_true(is.na(punif(0.5, NA, 1)))
  expect_true(is.na(punif(0.5, 0, NA)))
  expect_true(is.na(punif(NA, 0, 1)))

  expect_true(is.na(ptriangle(0.5, NA, 1, 0.5)))
  expect_true(is.na(ptriangle(0.5, 0, NA, 0.5)))
  expect_true(is.na(ptriangle(0.5, 0, 1, NA)))
  expect_true(is.na(ptriangle(NA, 0, 1, 0.5)))

  # out of order
  expect_warning(expect_true(all(is.nan(punif(0.5, 1, 0)))))
  # a > c
  expect_warning(expect_true(all(is.nan(ptriangle(0.5, 5, 6, 4)))))
  # b < c
  expect_warning(expect_true(all(is.nan(qtriangle(0.5, 5, 6, 7)))))

  # Inf
  expect_warning(expect_true(is.nan(punif(0.5, 0, Inf))))
  expect_warning(expect_true(is.nan(punif(0.5, -Inf, 1))))

  expect_warning(expect_true(is.nan(ptriangle(0.5, -Inf, 1, 0.5))))
  expect_warning(expect_true(is.na(ptriangle(0.5, 0, Inf, 0.5))))

  # multi-parameters
  expect_equal(punif(c(0, 4, -1, 2), 0, 4), c(0, 1, 0, 0.5))
  expect_equal(ptriangle(c(1, 3, -1, .5), c(1, 0, -1, 0), c(3, 3, 3, 1),
                         c(2, 2, 2, .5)), c(0, 1, 0, .5))
})

test_that("ptriangle is not consistent with punif", {
  # punif does recylcing here
  # punif(4, c(1,2), c(2,3,4)) will return 3 numbers
  expect_error(ptriangle(c(1,2,3), c(1,2,3), c(4,5), c(1,2,3)))
})
