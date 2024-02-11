test_that("logM works", {
  xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)

  expect_true(abs(exp(logM(xtest, 0, 1, 1)) - 0.007) < 0.001)
  expect_true(abs(exp(logM(xtest, 0, 1, 2)) - 0.010) < 0.001)
  expect_true(abs(exp(logM(xtest, 0, 1, 3)) - 0.011) < 0.001)
  expect_true(abs(exp(logM(xtest, 0, 1, 4)) - 0.010) < 0.001)
  expect_true(abs(exp(logM(xtest, 0, 1, 5)) - 0.009) < 0.001)
  expect_true(abs(exp(logM(xtest, 0, 1, 6)) - 0.005) < 0.001)
  expect_true(abs(exp(logM(xtest, 0, 1, 7)) - 0.004) < 0.001)
  expect_true(abs(exp(logM(xtest, 0, 1, 8, debug = TRUE)) - 0.003) < 0.001)
})

test_that("rhat works", {
  xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)

  expect_equal(3, rhat(xtest, 0, 1))
  expect_equal(3, rhat(xtest, 0, 1, debug = TRUE))
})

test_that("triangle_mle_c_given_ab works", {
  xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)

  expect_equal(0.3, triangle_mle_c_given_ab(xtest, 0, 1)$c_hat)

  expect_equal(0.3, triangle_mle_c_given_ab(xtest, 0, 1, debug = TRUE)$c_hat)
})

test_that("nLL_triangle works", {
  xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)

  expect_equal(-sum(log(dtriangle(xtest, 0, 1, 0.3))),
               nLL_triangle(xtest, 0, 1, 0.3),
               tolerance = 1E-5)

  expect_equal(-log(1/2), nLL_triangle(1, 0, 2, 2, debug = TRUE))
  expect_equal(-log(1/2), nLL_triangle(1, 0, 2, 0))
  expect_equal(-log(1), nLL_triangle(1, 0, 2, 1))
})

test_that("gradient_nLL_triangle_given_c works", {
  xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)

  g <- gradient_nLL_triangle_given_c(xtest, 0, 1, 0.3)

  expect_equal(2, length(g))
  expect_true(g[1] < 0 & g[2] > 0)

  expect_equal(c(0, 0.5), gradient_nLL_triangle_given_c(1, 0, 2, 2))
  expect_equal(c(-0.5, 0), gradient_nLL_triangle_given_c(1, 0, 2, 0))
  expect_equal(c(-0.5, 0.5), gradient_nLL_triangle_given_c(1, 0, 2, 1))
})

test_that("hessian_nLL_triangle_given_c works", {
  xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)

  h <- hessian_nLL_triangle_given_c(xtest, 0, 1, 0.3)

  expect_equal(c(2, 2), dim(h))
  expect_true(all(diag(h) > 0))
  expect_equal(h[1,2], h[2,1])

  expect_equal(c(-0.25, 0.75, 0.75, -0.75), c(hessian_nLL_triangle_given_c(c(0,1,2), 0, 2, 2)))
  expect_equal(c(-0.75, 0.75, 0.75, -0.25), c(hessian_nLL_triangle_given_c(c(0,1,2), 0, 2, 0)))
  expect_equal(c(-1.75, 0.75, 0.75, -1.75), c(hessian_nLL_triangle_given_c(c(0,1,2), 0, 2, 1)))

  h <- hessian_nLL_triangle_given_c(xtest, 0, 0.81, 0.81)
  expect_equal(c(2, 2), dim(h))
  expect_equal(h[1,2], h[2,1])
})

test_that("triangle_mle_ab_given_c works", {
  xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)

  temp <- triangle_mle_ab_given_c(xtest, 0.3, debug = TRUE)

  expect_true(temp$a < min(xtest))
  expect_true(temp$b > max(xtest))
  expect_equal(0, temp$optim$convergence)
  expect_equal(c(2, 2), dim(temp$hessian_ab))
  expect_true(all(diag(temp$hessian_ab) > 0))
  expect_equal(temp$hessian_ab[1,2], temp$hessian_ab[2,1])
})

test_that("f_rth_order_stat works", {
  temp <- integrate(f_rth_order_stat, lower = 0, upper = 1, n = 10, r = 3, a = 0, b = 1, c = 0.5)
  expect_equal(1, temp$value)

  expect_equal(rep(0, 4), f_rth_order_stat(c(-1, 0, 1, 2), 10, 3, 0, 1, 0.5))

  expect_true(all(f_rth_order_stat(seq(0.1, 0.9, length=1000), 10, 3, 0, 1, 0.5) > 0))

  expect_equal(rep(0, 4), f_rth_order_stat(c(1, 2, 0, -1), 5, 5, 0, 1, 0.5))
})

test_that("mean_rth_order_stat works", {
  temp <- mean_rth_order_stat(10, 5, 0, 1, 0.5)
  expect_true(temp >= 0 & temp <= 1)
  expect_true(temp < 0.5)

  temp <- mean_rth_order_stat(10, 6, 0, 1, 0.5)
  expect_true(temp > 0.5)

  temp <- mean_rth_order_stat(10, 1, 0, 1, 0.5)
  expect_true(temp >= 0 & temp <= 1)

  temp <- mean_rth_order_stat(10, 10, 0, 1, 0.5)
  expect_true(temp >= 0 & temp <= 1)

  expect_equal(mean_rth_order_stat_numeric(10, 5, 1, 3, 2),
               mean_rth_order_stat(10, 5, 1, 3, 2), tolerance = 1E-9)
  expect_equal(mean_rth_order_stat_numeric(10, 5, 1, 3, 1),
               mean_rth_order_stat(10, 5, 1, 3, 1), tolerance = 1E-9)
  expect_equal(mean_rth_order_stat_numeric(10, 5, 1, 3, 3),
               mean_rth_order_stat(10, 5, 1, 3, 3), tolerance = 1E-9)
  expect_equal(mean_rth_order_stat_numeric(10, 1, 1, 3, 2),
               mean_rth_order_stat(10, 1, 1, 3, 2), tolerance = 1E-9)
  expect_equal(mean_rth_order_stat_numeric(10, 10, 1, 3, 2),
               mean_rth_order_stat(10, 10, 1, 3, 2), tolerance = 1E-9)

  expect_true(mean_rth_order_stat(200, 100, 0, 1, 0.5) < 1)
  expect_true(mean_rth_order_stat(200, 100, 0, 1, 0.5) > 0)
})

test_that("mean_rth_order_stat matches multiple precision", {
  testthat::skip_if_not_installed("Rmpfr")

  expect_equal(mean_rth_order_stat(10, 5, 0, 1, 0.5),
               mean_rth_order_stat_rmpfr(10, 5, 0, 1, 0.5), tolerance = 1E-9)

  # a < c < b
  for (r in 1:10) {
    expect_equal(mean_rth_order_stat(10, r, 1, 3, 2),
                 mean_rth_order_stat_rmpfr(10, r, 1, 3, 2), tolerance = 1E-4)
  }
  # a == c < b
  for (r in 1:10) {
    expect_equal(mean_rth_order_stat(10, r, 1, 3, 1),
                 mean_rth_order_stat_rmpfr(10, r, 1, 3, 1), tolerance = 1E-9)
  }
  # a < c == b
  for (r in 1:10) {
    expect_equal(mean_rth_order_stat(10, r, 1, 3, 3),
                 mean_rth_order_stat_rmpfr(10, r, 1, 3, 3), tolerance = 1E-9)
  }

  expect_equal(mean_rth_order_stat_base(10, 5, 0, 1, 0.5),
               mean_rth_order_stat_rmpfr(10, 5, 0, 1, 0.5), tolerance = 1E-9)

  expect_equal(0.5, mean_rth_order_stat_base(11, 6, 0, 1, 0.5))
  expect_equal(0.5, mean_rth_order_stat_numeric(11, 6, 0, 1, 0.5))
  expect_equal(0.5, mean_rth_order_stat_rmpfr(11, 6, 0, 1, 0.5))
})

test_that("variance_rth_order_stat works", {
  expect_true(variance_rth_order_stat(10, 1, 0, 1, 0.5) >= 0)
  expect_true(variance_rth_order_stat(10, 2, 0, 1, 0.5) >= 0)
  expect_true(variance_rth_order_stat(10, 5, 0, 1, 0.5) >= 0)
  expect_true(variance_rth_order_stat(10, 8, 0, 1, 0.5) >= 0)
  expect_true(variance_rth_order_stat(10, 9, 0, 1, 0.5) >= 0)
  expect_true(variance_rth_order_stat(10, 10, 0, 1, 0.5) >= 0)

  expect_equal(variance_rth_order_stat_numeric(10, 5, 1, 3, 2),
               variance_rth_order_stat(10, 5, 1, 3, 2), tolerance = 1E-9)
  expect_equal(variance_rth_order_stat_numeric(10, 5, 1, 3, 1),
               variance_rth_order_stat(10, 5, 1, 3, 1), tolerance = 1E-9)
  expect_equal(variance_rth_order_stat_numeric(10, 5, 1, 3, 3),
               variance_rth_order_stat(10, 5, 1, 3, 3), tolerance = 1E-9)
  expect_equal(variance_rth_order_stat_numeric(10, 1, 1, 3, 2),
               variance_rth_order_stat(10, 1, 1, 3, 2), tolerance = 1E-9)
  expect_equal(variance_rth_order_stat_numeric(10, 10, 1, 3, 2),
               variance_rth_order_stat(10, 10, 1, 3, 2), tolerance = 1E-9)
})

test_that("variance_rth_order_stat matches multiple precision", {
  testthat::skip_if_not_installed("Rmpfr")

  expect_equal(variance_rth_order_stat(10, 5, 0, 1, 0.5),
               variance_rth_order_stat_rmpfr(10, 5, 0, 1, 0.5), tolerance = 1E-9)

  # a < c < b
  for (r in 1:10) {
    expect_equal(variance_rth_order_stat(10, r, 1, 3, 2),
                 variance_rth_order_stat_rmpfr(10, r, 1, 3, 2), tolerance = 1E-3)
  }
  # a == c < b
  for (r in 1:10) {
    expect_equal(variance_rth_order_stat(10, r, 1, 3, 1),
                 variance_rth_order_stat_rmpfr(10, r, 1, 3, 1), tolerance = 1E-9)
  }
  # a < c == b
  for (r in 1:10) {
    expect_equal(variance_rth_order_stat(10, r, 1, 3, 3),
                 variance_rth_order_stat_rmpfr(10, r, 1, 3, 3), tolerance = 1E-9)
  }
})

test_that("triangle_mle works", {
  xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)

  temp <- triangle_mle(xtest)

  expect_true(temp$coef[1] < min(xtest))
  expect_true(temp$coef[2] > max(xtest))
  expect_equivalent(0.3, temp$coef[3])

  expect_equal(c(3, 3), dim(temp$vcov))
  expect_true(all(diag(temp$vcov) >= 0))
  expect_equal(temp$vcov[1,2], temp$vcov[2,1])

  expect_equal(8, temp$nobs)
  expect_true(all(temp$x == xtest))

  expect_equal("triangle_mle", class(temp))

  expect_equivalent(nLL_triangle(xtest, temp$coef[1], temp$coef[2], temp$coef[3]),
               temp$min)
  expect_equal(0, temp$details$convergence)
})

test_that("standard_triangle_mle works", {
  xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
  temp <- standard_triangle_mle(xtest)

  expect_equivalent(0, temp$coef[1])
  expect_equivalent(1, temp$coef[2])
  expect_equivalent(0.3, temp$coef[3])

  expect_equal(c(3, 3), dim(temp$vcov))
  expect_true(all(diag(temp$vcov) >= 0))
  expect_equal(temp$vcov[1,2], temp$vcov[2,1])

  expect_equal(8, temp$nobs)
  expect_true(all(temp$x == xtest))

  expect_equal("triangle_mle", class(temp))

  expect_equivalent(nLL_triangle(xtest, temp$coef[1], temp$coef[2], temp$coef[3]),
                    temp$min)
})

