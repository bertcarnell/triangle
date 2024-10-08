#' @include dtriangle.R
#' @rdname triangle
#' @export
ptriangle <- function(q, a=0, b=1, c=(a + b)/2) {
  q1 <- length(q)
  a1 <- length(a)
  b1 <- length(b)
  c1 <- length(c)

  pTest <- function(X){
    if (any(is.na(X)))
    { # is.na is TRUE for NA, NaN, and FALSE
      if (any(is.nan(X)))
      {
        return(NaN) # to conform to punif

      } else
      {
        return(NA) # to conform to punif
      }
    } else if (X[2] > X[4] | X[3] < X[4] | (X[2] == X[4] & X[3] == X[4]))
    {
      warning("values required to be  a <= c <= b (at least one strict inequality)")
      return(NaN) # to conform to behavior of punif

    } else if (any(is.infinite(X[2:4])))
    {
      warning("a, b, c must be finite, NaN produced")
      return(NaN) # to conform to behavior of punif
    } else if (X[1] <= X[2]) {
      return(0)
    } else if (X[2] != X[4] & X[1] < X[4]) {
      return((X[1] - X[2])^2 / (X[3] - X[2]) / (X[4] - X[2]))
    } else if (X[4] != X[3] & X[1] >= X[4] & X[1] < X[3]) {
      return(1 - (X[3] - X[1])^2 / (X[3] - X[2]) / (X[3] - X[4]))
    } else if (X[1] >= X[3]) {
      return(1)
    }
  }

  k <- max(q1, a1, b1, c1)
  if (k == 1) return(pTest(c(q, a, b, c)))

  params <- matrix(nrow = k, ncol = 4)
  tryCatch(
  {
    params[,1] <- q
    params[,2] <- a
    params[,3] <- b
    params[,4] <- c
  }, error = function(X) {
    stop(paste(" -- Argument Lengths: length of q = ", q1,
                ", a = ", a1, ", b = ", b1, ", c = ", c1, " -- ", X, sep = ""))
  })

  return(apply(params, 1, pTest))
}
