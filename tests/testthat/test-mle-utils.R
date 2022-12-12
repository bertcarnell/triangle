test_that("summary works", {
  set.seed(39854)
  xtest <- rtriangle(200, 0, 1, 0.5)

  mle1 <- triangle_mle(xtest)
  summ <- summary(mle1)

  expect_equal(summ@coef[,1], coef(mle1))
  expect_equal(summ@coef[,2], sqrt(diag(vcov(mle1))))

  expect_equivalent(2 * nLL_triangle(xtest, mle1$coef[1], mle1$coef[2], mle1$coef[3]),
               summ@m2logL)
})

test_that("print works", {
  set.seed(39854)
  xtest <- rtriangle(200, 0, 1, 0.5)

  mle1 <- triangle_mle(xtest)
  expect_output(print(mle1))
})

test_that("coef works", {
  set.seed(39854)
  xtest <- rtriangle(200, 0, 1, 0.5)

  mle1 <- triangle_mle(xtest)
  cf <- coef(mle1)

  expect_equal(mle1$coef, cf)
})

test_that("logLik works", {
  set.seed(39854)
  xtest <- rtriangle(200, 0, 1, 0.5)

  mle1 <- triangle_mle(xtest)
  ll <- logLik(mle1)

  expect_equivalent(-1*nLL_triangle(xtest, mle1$coef[1], mle1$coef[2], mle1$coef[3]),
                    as.numeric(ll))
  expect_equal("logLik", class(ll))
  expect_equal(3, attr(ll, "df"))
})

test_that("AIC works", {
  set.seed(39854)
  xtest <- rtriangle(200, 0, 1, 0.5)

  mle1 <- triangle_mle(xtest)
  aic <- AIC(mle1)

  expect_equivalent(2 * nLL_triangle(xtest, mle1$coef[1], mle1$coef[2], mle1$coef[3]) + 2 * 3,
                    aic)
})

test_that("BIC works", {
  set.seed(39854)
  xtest <- rtriangle(200, 0, 1, 0.5)

  mle1 <- triangle_mle(xtest)
  bic <- BIC(mle1)

  expect_equivalent(2 * nLL_triangle(xtest, mle1$coef[1], mle1$coef[2], mle1$coef[3]) + log(length(xtest)) * 3,
                    bic)
})

test_that("vcov works", {
  set.seed(39854)
  xtest <- rtriangle(200, 0, 1, 0.5)

  mle1 <- triangle_mle(xtest)
  v <- vcov(mle1)

  expect_equivalent(mle1$vcov, v)
})

test_that("profile works", {
  set.seed(39854)
  xtest <- rtriangle(200, 0, 1, 0.5)

  mle1 <- triangle_mle(xtest)
  prof <- profile(mle1)

  expect_equivalent("profile.mle", class(prof))
  expect_equal(3, length(prof@profile))
  expect_equal(3, ncol(prof@profile$a$par.vals))

  mle1 <- standard_triangle_mle(xtest)
  prof <- profile(mle1)

  expect_equivalent("profile.mle", class(prof))
  expect_equal(3, length(prof@profile))
  expect_equal(3, ncol(prof@profile$a$par.vals))
})

test_that("confint works", {
  set.seed(39854)
  xtest <- rtriangle(200, 0, 1, 0.5)

  mle1 <- triangle_mle(xtest)
  cfi <- confint(mle1, level = 0.95)

  expect_equal(c(3,2), dim(cfi))
  expect_true(cfi[1,1] < 0 & 0 < cfi[1,2])
  expect_true(cfi[2,1] < 1 & 1 < cfi[2,2])

  cfi <- confint(mle1, level = 0.9)

  expect_true(cfi[1,1] < 0 & 0 < cfi[1,2])
  expect_true(cfi[2,1] < 1 & 1 < cfi[2,2])
  expect_true(cfi[3,1] < 0.5 & 0.5 < cfi[3,2])
})

