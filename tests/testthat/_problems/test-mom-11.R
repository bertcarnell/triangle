# Extracted from test-mom.R:11

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "triangle", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
set.seed(103930)
x <- rtriangle(20, 0, 2, 1.5)
mom <- triangle_mom(x)
expect_equal(3, length(mom))
expect_true(mom[1] <= mom[3])
expect_true(mom[2] >= mom[3])
expect_true(mom[1] <= mom[2])
mom2 <- triangle_mom(x, type = 1)
