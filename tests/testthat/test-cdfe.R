test_that("triangle_cdfe works", {
  set.seed(10304)
  xtest2 <- rtriangle(100, 1, 5, 2)
  cdfe <- triangle_cdfe(xtest2)

  expect_equal("nls", class(cdfe))
  expect_output(print(cdfe))
  expect_output(print(summary(cdfe)))
  expect_equal(3, length(coef(cdfe)))
  expect_true(coef(cdfe)[1] < coef(cdfe)[2])
  expect_true(coef(cdfe)[1] <= min(xtest2))
  expect_true(coef(cdfe)[2] >= max(xtest2))
  expect_true(coef(cdfe)[3] <= coef(cdfe)[2])
  expect_true(coef(cdfe)[3] >= coef(cdfe)[1])
  expect_true(coef(cdfe)[3] >= min(xtest2))
  expect_true(coef(cdfe)[3] <= max(xtest2))

  # do this so MASS:::confint.nls can be used
  if (requireNamespace("MASS", quietly = TRUE)) {
    expect_equal(c(3, 2), dim(confint(cdfe)))
  }
})
