test_that("method of moments works", {
  set.seed(103930)
  x <- rtriangle(20, 0, 2, 1.5)
  mom <- triangle_mom(x)

  expect_equal(3, length(mom))
  expect_true(mom[1] <= mom[3])
  expect_true(mom[2] >= mom[3])
  expect_true(mom[1] <= mom[2])
})
