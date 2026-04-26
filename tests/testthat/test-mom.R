test_that("method of moments works", {
  set.seed(103930)
  x <- rtriangle(20, 0, 2, 1.5)
  mom <- triangle_mom(x)

  expect_equal(3, length(mom))
  expect_true(mom[1] <= mom[3])
  expect_true(mom[2] >= mom[3])
  expect_true(mom[1] <= mom[2])

  mom2 <- triangle_mom(x, type = 1)

  expect_equal(mom, mom2)

  mom3 <- triangle_mom(x, type = 2)
  expect_equal(3, length(mom))
  expect_true(mom3[1] <= mom3[3])
  expect_true(mom3[2] >= mom3[3])
  expect_true(mom3[1] <= mom3[2])
  expect_true(abs(mean(x) -(mom3["a"] + mom3["b"] + mom3["c"])/3) < 0.1)

  x <- rtriangle(20, 1, 3, 1)
  mom3 <- triangle_mom(x, type = 2)
  expect_equal(3, length(mom))
  expect_true(mom3[1] <= mom3[3])
  expect_true(mom3[2] >= mom3[3])
  expect_true(mom3[1] <= mom3[2])
  expect_true(abs(mean(x) -(mom3["a"] + mom3["b"] + mom3["c"])/3) < 0.1)

  x <- rtriangle(20, 1, 3, 3)
  mom3 <- triangle_mom(x, type = 2)
  expect_equal(3, length(mom))
  expect_true(mom3[1] <= mom3[3])
  expect_true(mom3[2] >= mom3[3])
  expect_true(mom3[1] <= mom3[2])
  expect_true(abs(mean(x) -(mom3["a"] + mom3["b"] + mom3["c"])/3) < 0.1)
})

test_that("repeated mom", {
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
  expect_true(all(abs(apply(b, 1, mean) - c(a=0, b=2, c=1.5)) < 0.2))

  set.seed(1393)
  b <- replicate(200, {
    x <- rtriangle(100, 0, 2, 2)
    triangle_mom(x)
  })
  expect_true(all(abs(apply(b, 1, mean) - c(a=0, b=2, c=2)) < 0.2))

  set.seed(1393)
  b <- replicate(200, {
    x <- rtriangle(100, 0, 2, 2)
    triangle_mom(x, type = 2)
  })
  expect_true(all(abs(apply(b, 1, mean) - c(a=0, b=2, c=2)) < 0.2))

})
