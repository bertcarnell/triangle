if (require(testthat)) {
  library(triangle)

  test_check("triangle")
} else {
  cat("\ntestthat is not available\n")
}
