test_that("logM works", {
  expect_true(abs(exp(logM(xtest_small, 0, 1, 1)) - 0.007) < 0.001)
  expect_true(abs(exp(logM(xtest_small, 0, 1, 2)) - 0.010) < 0.001)
  expect_true(abs(exp(logM(xtest_small, 0, 1, 3)) - 0.011) < 0.001)
  expect_true(abs(exp(logM(xtest_small, 0, 1, 4)) - 0.010) < 0.001)
  expect_true(abs(exp(logM(xtest_small, 0, 1, 5)) - 0.009) < 0.001)
  expect_true(abs(exp(logM(xtest_small, 0, 1, 6)) - 0.005) < 0.001)
  expect_true(abs(exp(logM(xtest_small, 0, 1, 7)) - 0.004) < 0.001)
  expect_true(abs(exp(logM(xtest_small, 0, 1, 8, debug = TRUE)) - 0.003) < 0.001)
})

test_that("rhat works", {
  expect_equal(3, rhat(xtest_small, 0, 1))
  expect_equal(3, rhat(xtest_small, 0, 1, debug = TRUE))
})

test_that("triangle_mle_c_given_ab works", {
  expect_equal(0.3, triangle_mle_c_given_ab(xtest_small, 0, 1)$c_hat)

  expect_equal(0.3, triangle_mle_c_given_ab(xtest_small, 0, 1, debug = TRUE)$c_hat)
})

test_that("nLL_triangle works", {
  expect_equal(-sum(log(dtriangle(xtest_small, 0, 1, 0.3))),
               nLL_triangle(xtest_small, 0, 1, 0.3),
               tolerance = 1E-5)

  expect_equal(-log(1/2), nLL_triangle(1, 0, 2, 2, debug = TRUE))
  expect_equal(-log(1/2), nLL_triangle(1, 0, 2, 0))
  expect_equal(-log(1), nLL_triangle(1, 0, 2, 1))

  # when a = c < b, then the mle of a is min(x)
  expect_equal(-log(dtriangle(0, 0, 2, 0)), nLL_triangle(0, 0, 2, 0))
  expect_equal(-log(dtriangle(0, 0, 2, 0)*dtriangle(1, 0, 2, 0)), nLL_triangle(c(0,1), 0, 2, 0))
  # when a < c = b, then the mle of b is max(x)
  expect_equal(-log(dtriangle(2, 0, 2, 2)), nLL_triangle(2, 0, 2, 2))
  expect_equal(-log(dtriangle(2, 0, 2, 2)*dtriangle(1, 0, 2, 2)), nLL_triangle(c(2,1), 0, 2, 2))
  # when a < c < b, then the mle of a or b sill might be at the min and max
  expect_equal(-log(dtriangle(0, 0, 2, 1)), nLL_triangle(0, 0, 2, 1))
  expect_equal(-log(dtriangle(0, 0, 2, 1)*dtriangle(1, 0, 2, 1)), nLL_triangle(c(0,1), 0, 2, 1))
  expect_equal(-log(dtriangle(2, 0, 2, 1)), nLL_triangle(2, 0, 2, 1))
  expect_equal(-log(dtriangle(2, 0, 2, 1)*dtriangle(1, 0, 2, 1)), nLL_triangle(c(2,1), 0, 2, 1))

  # check that the nLL also integrates to 1 for each type of triangle
  int1 <- integrate(f = function(x) sapply(x, function(xi) exp(-1*nLL_triangle(xi, 0, 2, 1))), lower = 0, upper = 2)
  expect_equal(1, int1$value, tolerance = int1$abs.error)
  int1 <- integrate(f = function(x) sapply(x, function(xi) exp(-1*nLL_triangle(xi, 0, 2, 2))), lower = 0, upper = 2)
  expect_equal(1, int1$value, tolerance = int1$abs.error)
  int1 <- integrate(f = function(x) sapply(x, function(xi) exp(-1*nLL_triangle(xi, 0, 2, 0))), lower = 0, upper = 2)
  expect_equal(1, int1$value, tolerance = int1$abs.error)
})

test_that("gradient_nLL_triangle_given_c works", {
  g <- gradient_nLL_triangle_given_c(xtest_small, 0, 1, 0.3)

  expect_equal(2, length(g))
  expect_true(g[1] < 0 & g[2] > 0)

  expect_equal(c(0, 0.5), gradient_nLL_triangle_given_c(1, 0, 2, 2))
  expect_equal(c(-0.5, 0), gradient_nLL_triangle_given_c(1, 0, 2, 0))
  expect_equal(c(-0.5, 0.5), gradient_nLL_triangle_given_c(1, 0, 2, 1))
})

test_that("hessian_nLL_triangle_given_c works", {
  h <- hessian_nLL_triangle_given_c(xtest_small, 0, 1, 0.3)

  expect_equal(c(2, 2), dim(h))
  expect_true(all(diag(h) > 0))
  expect_equal(h[1,2], h[2,1])

  expect_equal(c(-0.25, 0.75, 0.75, -0.75), c(hessian_nLL_triangle_given_c(c(0,1,2), 0, 2, 2)))
  expect_equal(c(-0.75, 0.75, 0.75, -0.25), c(hessian_nLL_triangle_given_c(c(0,1,2), 0, 2, 0)))
  expect_equal(c(-1.75, 0.75, 0.75, -1.75), c(hessian_nLL_triangle_given_c(c(0,1,2), 0, 2, 1)))

  h <- hessian_nLL_triangle_given_c(xtest_small, 0, 0.81, 0.81)
  expect_equal(c(2, 2), dim(h))
  expect_equal(h[1,2], h[2,1])
})

test_that("triangle_mle_ab_given_c works", {
  temp <- triangle_mle_ab_given_c(xtest_small, 0.3, debug = TRUE)

  expect_true(temp$a < min(xtest_small))
  expect_true(temp$b > max(xtest_small))
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
  temp <- triangle_mle(xtest_small)

  expect_true(temp$coef[1] <= min(xtest_small))
  expect_true(temp$coef[2] >= max(xtest_small))
  # expect_equivalent(0.3, temp$coef[3]) # this isn't true under the left skewwed triangle allowance

  expect_equal(c(3, 3), dim(temp$vcov))
  expect_true(all(diag(temp$vcov) >= 0) | all(is.na(diag(temp$vcov))))  ### wouild like to do better here
  expect_equal(temp$vcov[1,2], temp$vcov[2,1])

  expect_equal(8, temp$nobs)
  expect_true(all(temp$x == xtest_small))

  expect_equal("triangle_mle", class(temp))

  expect_equivalent(nLL_triangle(xtest_small, temp$coef[1], temp$coef[2], temp$coef[3]),
               temp$min)
  expect_equal(0, temp$details$convergence)

  # check that triangles that cross 0 are fine
  set.seed(19393)
  xs <- rtriangle(40, -1, 5, 4)
  expect_no_error(triangle_mle(xs))

  xs <- rtriangle(40, -5, 4,-1)
  expect_no_error(mle2 <- triangle_mle(xs))
  ci <- confint(mle2)
  expect_gte(ci[1,2], -5)
  expect_lte(ci[1,1], -5)
  expect_gte(ci[2,2], 4)
  expect_lte(ci[2,1], 4)
  expect_gte(ci[3,2], -1)
  expect_lte(ci[3,1], -1)

  xs <- rtriangle(40, -5, -1, -3)
  expect_no_error(mle2 <- triangle_mle(xs))
})

test_that("standard_triangle_mle works", {
  temp <- standard_triangle_mle(xtest_small)

  expect_equivalent(0, temp$coef[1])
  expect_equivalent(1, temp$coef[2])
  expect_equivalent(0.3, temp$coef[3])

  expect_equal(c(3, 3), dim(temp$vcov))
  expect_true(all(diag(temp$vcov) >= 0))
  expect_equal(temp$vcov[1,2], temp$vcov[2,1])

  expect_equal(8, temp$nobs)
  expect_true(all(temp$x == xtest_small))

  expect_equal("triangle_mle", class(temp))

  expect_equivalent(nLL_triangle(xtest_small, temp$coef[1], temp$coef[2], temp$coef[3]),
                    temp$min)
})

test_that("breakdown nLL problems", {
  mom1 <- triangle_mom(xtest_small, type = 1)
  triangle:::nLL_triangle(xtest_small, mom1["a"], mom1["b"], mom1["c"]) # inf because a=min(x) and b=max(x)

  # start with mom type 2
  mom2 <- triangle_mom(xtest_small, type = 2)
  triangle:::nLL_triangle(xtest_small, mom2["a"], mom2["b"], mom2["c"])
  # optimize from there
  mle_c0 <- triangle:::triangle_mle_c_given_ab(xtest_small, mom2["a"], mom2["b"])
  mle_ab0 <- triangle:::triangle_mle_ab_given_c(xtest_small, mle_c0$c_hat, start = c(mom2["a"], mom2["b"]))
  mle_c1 <- triangle:::triangle_mle_c_given_ab(xtest_small, mle_ab0$a, mle_ab0$b)
  mle_ab1 <- triangle:::triangle_mle_ab_given_c(xtest_small, mle_c1$c_hat, start = c(mle_ab0$a, mle_ab0$b))
  mle_c2 <- triangle:::triangle_mle_c_given_ab(xtest_small, mle_ab1$a, mle_ab1$b)
  mle_ab2 <- triangle:::triangle_mle_ab_given_c(xtest_small, mle_c2$c_hat, start = c(mle_ab1$a, mle_ab1$b))
  triangle:::nLL_triangle(xtest_small, mle_ab0$a, mle_ab0$b, mle_c0$c_hat)
  triangle:::nLL_triangle(xtest_small, mle_ab1$a, mle_ab1$b, mle_c1$c_hat)
  triangle:::nLL_triangle(xtest_small, mle_ab2$a, mle_ab2$b, mle_c2$c_hat)
  # end up with c at 0.3 and a around 0 and b around 1
  expect_equal(mle_c1$c_hat, mle_c2$c_hat)
  expect_equal(mle_ab1$optim$value, mle_ab2$optim$value)
  # BIC
  3*log(length(xtest_small)) - 2*-1*triangle:::nLL_triangle(xtest_small, mle_ab2$a, mle_ab2$b, mle_c2$c_hat)
  # AIC
  2*3 - 2*-1*triangle:::nLL_triangle(xtest_small, mle_ab2$a, mle_ab2$b, mle_c2$c_hat)
  prod(dtriangle(xtest_small, mle_ab2$a, mle_ab2$b, mle_c2$c_hat))

  # however, the triangle with a = -0.1 and b = c = 0.8 has a smaller nLL and smaller BIC
  mle <- triangle_mle(xtest_small)
  triangle:::nLL_triangle(xtest_small, coef(mle)["a"], coef(mle)["b"], coef(mle)["c"])
  # BIC
  2*log(length(xtest_small)) - 2*-1*triangle:::nLL_triangle(xtest_small, coef(mle)["a"], coef(mle)["b"], coef(mle)["c"])
  # AIC
  2*2 - 2*-1*triangle:::nLL_triangle(xtest_small, coef(mle)["a"], coef(mle)["b"], coef(mle)["c"])
  prod(dtriangle(xtest_small, coef(mle)["a"], coef(mle)["b"], coef(mle)["c"]))

})
