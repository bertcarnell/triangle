# Extracted from test-mom.R:53

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "triangle", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_on_cran()
skip_on_ci()
set.seed(1393)
b <- replicate(200, {
    x <- rtriangle(100, 0, 2, 1.5)
    triangle_mom(x)
  })
expect_true(all(abs(apply(b, 1, mean) - c(a=0, b=2, c=1.5)) < 0.2))
set.seed(1393)
b <- replicate(200, {
    x <- rtriangle(100, 0, 2, 1.5)
    triangle_mom(x, type = 2)
  })
