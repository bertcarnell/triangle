# Extracted from test-mle.R:264

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "triangle", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
mom1 <- triangle_mom(xtest_small, type = 1)
