set.seed(39854)
xtest <- rtriangle(200, 0, 1, 0.5)

mle1 <- triangle_mle(xtest)
