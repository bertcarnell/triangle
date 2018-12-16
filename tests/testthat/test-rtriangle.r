context("test-rtriangle")

test_that("multiplication works", {
  expect_true(max(range(rtriangle(100))) <= 1)
  expect_true(min(range(rtriangle(100))) >= 0)
  expect_true(length(rtriangle(20.3)) == 20)

  expect_error(rtriangle(-5))
  expect_equal(rtriangle(2, NaN, 3, 1), c(NaN, NaN))
  expect_equal(rtriangle(1, 3, NA, 4), NaN)
  expect_error(rtriangle(NA))

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
  expect_true(all(1 == rtriangle(10,1,1,1)))
  expect_true(all(-1 == rtriangle(10,-1,-1,-1)))
})
