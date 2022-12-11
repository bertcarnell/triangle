test_that("qqtriangle works", {
  xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
  theta <- coef(triangle_mle(xtest))
  expect_no_error(qqtriangle(xtest, theta[1], theta[2], theta[3]))
})

test_that("compare_triangle_fit works", {
  xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
  expect_no_error(compare_triangle_fit(xtest))
})

