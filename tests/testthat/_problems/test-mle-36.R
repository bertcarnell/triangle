# Extracted from test-mle.R:36

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "triangle", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
expect_equal(-sum(log(dtriangle(xtest_small, 0, 1, 0.3))),
               nLL_triangle(xtest_small, 0, 1, 0.3),
               tolerance = 1E-5)
expect_equal(-log(1/2), nLL_triangle(1, 0, 2, 2, debug = TRUE))
expect_equal(-log(1/2), nLL_triangle(1, 0, 2, 0))
expect_equal(-log(1), nLL_triangle(1, 0, 2, 1))
expect_equal(-log(dtriangle(0, 0, 2, 0)), nLL_triangle(0, 0, 2, 0))
expect_equal(-log(dtriangle(0, 0, 2, 0)*dtriangle(1, 0, 2, 0)), nLL_triangle(c(0,1), 0, 2, 0))
expect_equal(-log(dtriangle(2, 0, 2, 2)), nLL_triangle(2, 0, 2, 2))
