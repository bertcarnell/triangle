test_that("qqtriangle works", {
  set.seed(10304)
  xtest <- rtriangle(100, 1, 5, 2)
  theta <- coef(triangle_mle(xtest))
  expect_no_error(qqtriangle(xtest, theta[1], theta[2], theta[3]))
})

test_that("compare_triangle_fit works", {
  set.seed(10304)
  xtest <- rtriangle(100, 1, 5, 2)
  expect_no_error(compare_triangle_fit(xtest))
})

