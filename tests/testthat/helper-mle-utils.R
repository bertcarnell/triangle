set.seed(39854)
xtest <- rtriangle(200, 0, 1, 0.5)

mle1 <- triangle_mle(xtest)

xtest_small <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
