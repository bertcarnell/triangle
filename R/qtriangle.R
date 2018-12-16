# Copyright 2018 Rob Carnell

#' @include dtriangle.R
#' @rdname triangle
#' @export
qtriangle <- function(p, a=0, b=1, c=(a + b)/2) {
  p1 <- length(p)
  a1 <- length(a)
  b1 <- length(b)
  c1 <- length(c)

  qTest <- function(X){
    # X = c(p, a, b, c)
    if (any(is.na(X))) { # is.na is TRUE for NA, NaN, and FALSE
      if (any(is.nan(X))) return(NaN) # to conform to qunif
      else return(NA) # to conform to qunif
    } else if (X[2] > X[4] | X[3] < X[4]) {
      warning("values required to be  a <= c <= b (at least one strict inequality)")
      return(NaN) # to conform to behavior of qunif
    } else if (X[1] < 0 | X[1] > 1) {
      warning("at least one p is outside [0,1]")
      return(NaN) # to conform to behavior of qunif
    } else if (any(is.infinite(X))) {
      return(NaN)
    } else if ((X[2] != X[4] &&
               (X[2] + sqrt(X[1]*(X[3] - X[2])*(X[4] - X[2]))) <= X[4]) |
              (X[2] == X[4] &&
               (X[2] + sqrt(X[1]*(X[3] - X[2])*(X[4] - X[2]))) < X[4])) {
      return(X[2] + sqrt(X[1]*(X[3] - X[2])*(X[4] - X[2])))
    } else if ((X[2] != X[4] &&
               (X[3] - sqrt((1 - X[1])*(X[3] - X[2])*(X[3] - X[4]))) > X[4]) |
              (X[2] == X[4] &&
               (X[3] - sqrt((1 - X[1])*(X[3] - X[2])*(X[3] - X[4]))) >= X[4])) {
      return(X[3] - sqrt((1 - X[1])*(X[3] - X[2])*(X[3] - X[4])))
    } else if (X[2] != X[4] && (X[2] + sqrt(X[1]*(X[3] - X[2])*(X[4] - X[2]))) - X[4] <= .Machine$double.eps)
    {
      return(X[4])
    } else stop("Unexpected Result")
  }

  k <- max(p1, a1, b1, c1)
  if (k == 1) return(qTest(c(p,a,b,c)))

  params <- matrix(nrow = k, ncol = 4)
  tryCatch(
  {
    params[,1] <- p
    params[,2] <- a
    params[,3] <- b
    params[,4] <- c
  }, error = function(X) {
    stop(paste(" -- Argument Lengths: length of p = ", p1,
                ", a = ", a1, ", b = ", b1, ", c = ", c1, " -- ", X, sep = ""))
  })

  return(apply(params, 1, qTest))
}
