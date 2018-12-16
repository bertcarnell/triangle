context("test-ltriangle")

test_that("rltriangle",
{
  expect_true(max(range(rltriangle(100))) <= 100)
  expect_true(min(range(rltriangle(100))) >= 1)
  expect_true(length(rltriangle(20.3)) == 20)

  expect_error(rltriangle(-5))
  expect_equal(rltriangle(2, NaN, 3, 1), c(NaN, NaN))
  expect_equal(rltriangle(1, 3, NA, 4), NaN)
  expect_error(rltriangle(NA))

  test <- rltriangle(1, 2, 5, 2 + 10^-6)
  expect_true(test > 2 & test < 5)
  test <- rltriangle(1, 2, 5, 2 + 10^-9)
  expect_true(test > 2 & test < 5)
  test <- rltriangle(1, 2, 5, 2 + 10^-12)
  expect_true(test > 2 & test < 5)
  expect_true(!is.nan(rltriangle(1, 2, 5, 2 + 10^-18)))

  expect_true(!all(1 == rltriangle(100, 1, 100, 1)))
  expect_true(!any(is.nan(rltriangle(100, 10, 100, 10))))
  expect_true(all(!is.finite(rltriangle(10, -1, 0, -1))))
  expect_true(all(is.na(rltriangle(10, -5, -2, -2))))
  expect_true(all(1 == rltriangle(10,1,1,1)))
  expect_true(all(is.na(rltriangle(10,-1,-1,-1))))
})

test_that("dltriangle",
{
  expect_equal(dltriangle(1, 1, 3, 2), 0)
  expect_equal(dltriangle(3, 1, 3, 2), 0)
  expect_equal(dltriangle(10^0.5, 1, 10, 10^0.5), 2)
  expect_warning(expect_equal(dltriangle(0, 0, 0, 0), NaN))
  expect_warning(expect_equal(dltriangle(.5, 0, 1, 2), NaN))
  expect_warning(expect_equal(dltriangle(.5, 1, 2, .5), NaN))
  expect_equal(dltriangle(c(1, 3, 10^.5), c(1, 1, 1), c(3, 3, 10),
                                c(2, 2, 10^.5)), c(0, 0, 2))
})

test_that("ptriangle",
{
  expect_equal(pltriangle(1, 1, 3, 2), 0)
  expect_equal(pltriangle(3, 1, 3, 2), 1)
  expect_equal(pltriangle(10^.5, 1, 10, 10^.5), .5)
  expect_warning(expect_equal(pltriangle(0, 0, 0, 0), NaN))
  expect_warning(expect_equal(pltriangle(.5, 0, 1, 2), NaN))
  expect_warning(expect_equal(pltriangle(.5, 1, 2, .5), NaN))
  expect_equal(pltriangle(c(1, 3, 10^.5), c(1, 1, 1), c(3, 3, 10),
                                c(2, 2, 10^.5)), c(0, 1, .5))
})

test_that("qltriangle",
{
  expect_equal(qltriangle(0, 1, 3, 2), 1)
  expect_equal(qltriangle(0, 2, 3, 2), 2)
  expect_equal(qltriangle(1, 1, 3, 2), 3)
  expect_equal(qltriangle(.5, 1, 10, 10^.5), 10^.5)
  expect_equal(qltriangle(0, 1, 1, 1), 1)
  expect_warning(expect_equal(qltriangle(-1, 0, 1, .5), NaN))
  expect_warning(expect_equal(qltriangle(2, 0, 1, .5), NaN))
  expect_warning(expect_equal(qltriangle(.5, 0, 1, 2), NaN))
  expect_warning(expect_equal(qltriangle(.5, 1, 2, .5), NaN))
  expect_equal(qltriangle(.5, NA, 2, .5), NA)
  expect_equal(qltriangle(c(0, 0, 1, .5), c(1, 2, 1, 1),
                                c(3, 3, 3, 10), c(2, 2, 2, 10^.5)),
                     c(1, 2, 3, 10^.5))
  expect_error(qltriangle(c(0, 0, 0, 0, 0), c(0, 0), 1, .5))
  expect_equal(qltriangle(.5, NaN, 2, 1), NaN)
  expect_equal(qltriangle(.5, 0, Inf, 1), NaN)
  expect_equal(qltriangle(.5, 0, Inf, Inf), NaN)

  expect_equal(qltriangle(pltriangle(15, 1, 100, 10), 1, 100, 10), 15)
  expect_equal(qltriangle(pltriangle(3, 2, 5, 5), 2, 5, 5), 3)
  expect_equal(qltriangle(pltriangle(5, 2, 5, 5), 2, 5, 5), 5)
})
