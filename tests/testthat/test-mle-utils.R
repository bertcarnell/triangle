test_that("summary works", {
  summ <- summary(mle1)

  expect_equal(summ@coef[,1], coef(mle1))
  expect_equal(summ@coef[,2], sqrt(diag(vcov(mle1))))

  expect_equivalent(2 * nLL_triangle(xtest, mle1$coef[1], mle1$coef[2], mle1$coef[3]),
               summ@m2logL)
})

test_that("print works", {
  expect_output(print(mle1))
})

test_that("coef works", {
  cf <- coef(mle1)

  expect_equal(mle1$coef, cf)
})

test_that("logLik works", {
  ll <- logLik(mle1)

  expect_equivalent(-1*nLL_triangle(xtest, mle1$coef[1], mle1$coef[2], mle1$coef[3]),
                    as.numeric(ll))
  expect_equal("logLik", class(ll))
  expect_equal(3, attr(ll, "df"))
})

test_that("AIC works", {
  aic <- AIC(mle1)

  expect_equivalent(2 * nLL_triangle(xtest, mle1$coef[1], mle1$coef[2], mle1$coef[3]) + 2 * 3,
                    aic)
})

test_that("BIC works", {
  bic <- BIC(mle1)

  expect_equivalent(2 * nLL_triangle(xtest, mle1$coef[1], mle1$coef[2], mle1$coef[3]) + log(length(xtest)) * 3,
                    bic)
})

test_that("vcov works", {
  v <- vcov(mle1)

  expect_equivalent(mle1$vcov, v)
})

test_that("profile works", {
  prof <- profile(mle1)

  expect_equivalent("profile.mle", class(prof))
  expect_equal(3, length(prof@profile))
  expect_equal(3, ncol(prof@profile$a$par.vals))

  mle2 <- standard_triangle_mle(xtest)
  prof <- profile(mle2)

  expect_equivalent("profile.mle", class(prof))
  expect_equal(3, length(prof@profile))
  expect_equal(3, ncol(prof@profile$a$par.vals))
})

test_that("confint works", {
  cfi <- confint(mle1, level = 0.95)

  expect_equal(c(3,2), dim(cfi))
  expect_true(cfi[1,1] < 0 & 0 < cfi[1,2])
  expect_true(cfi[2,1] < 1 & 1 < cfi[2,2])

  cfi <- confint(mle1, level = 0.9)

  expect_true(cfi[1,1] < 0 & 0 < cfi[1,2])
  expect_true(cfi[2,1] < 1 & 1 < cfi[2,2])
  expect_true(cfi[3,1] < 0.5 & 0.5 < cfi[3,2])
})

