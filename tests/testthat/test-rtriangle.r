context("test-rtriangle")

test_that("rtriangle works", {
  expect_true(max(rtriangle(100)) <= 1)
  expect_true(min(rtriangle(100)) >= 0)

  # From a Bug Report, Michael.Scroggie@dse.vic.gov.au, Thursday 10/19/06
  expect_true(!all(0 == rtriangle(1, 0, 1, 0)))
  expect_true(!all(1 == rtriangle(1, 0, 1, 1)))
  expect_true(!all(5 == rtriangle(1, 2, 5, 5)))
  expect_true(!identical(NaN, rtriangle(1, 2, 5, 2)))

  test <- rtriangle(1, 2, 5, 2 + 10^-6)
  expect_true(test > 2 & test < 5)
  test <- rtriangle(1, 2, 5, 2 + 10^-9)
  expect_true(test > 2 & test < 5)
  test <- rtriangle(1, 2, 5, 2 + 10^-12)
  expect_true(test > 2 & test < 5)
  expect_true(!is.nan(rtriangle(1, 2, 5, 2 + 10^-18)))

  expect_true(!all(0 == rtriangle(100, 0, 100, 0)))
  expect_true(!any(is.nan(rtriangle(100, 10, 100, 10))))
  expect_true(!all(-1 == rtriangle(10, -1, 0, -1)))
  expect_true(!all(-2 == rtriangle(10, -5, -2, -2)))
  expect_true(all(1 == rtriangle(10, 1, 1, 1)))
  expect_true(all(-1 == rtriangle(10, -1, -1, -1)))
})

test_that("rtriangle is consistent with runif", {
  # consistent with runif(20.3)
  expect_equal(length(rtriangle(20.3)), 20)
  # runif(c(1,2,3,6)) returns a vector of length 4
  expect_equal(length(rtriangle(c(1,2,3,4), 4, 5, 5)), 4)
  expect_equal(2, length(rtriangle(c(4,5), 0, 2, 1)))

  # runif(-1) errors
  expect_error(rtriangle(-5))
  # runif(2, NaN, 5) returns c(NaN, NaN) and warning
  expect_warning(expect_equal(rtriangle(2, NaN, 3, 1), c(NaN, NaN)))
  # runif(1, 3, NA) returns NaN and warning
  expect_warning(expect_equal(rtriangle(1, 3, NA, 4), NaN))
  # runif(NA) errors
  expect_error(rtriangle(NA))
  # runif(Inf) errors
  expect_error(rtriangle(Inf))

  # runif(1, 10, 3) produces NaN with warning
  # a > c
  expect_warning(expect_true(all(is.nan(rtriangle(10, 5, 6, 4)))))
  # b < c
  expect_warning(expect_true(all(is.nan(rtriangle(10, 5, 6, 7)))))
  # runif(1, 1, Inf) returns NaN with a warning
  expect_warning(expect_true(all(is.nan(rtriangle(10, 5, Inf, 7)))))
})

test_that("rtriangle is not consistent with runif", {
  # this is not how runif works, but runif would be difficult to implment
  # runif(4, c(1,2,3), c(1,2,3)) == c(1,2,3,1)
  expect_error(rtriangle(10, c(1,2), 4, 6))
})
