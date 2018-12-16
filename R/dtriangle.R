################################################################################
#
# Program:   dtriangle.R
# Purpose:   To calculate the PDF for the triangle distribution
# Author:    Rob Carnell
# Date:      June 05
#
# Variables
#   used the same naming conventions as other R distributions (q,p,d)
#   p = cumulative probability
#   a = left triangle endpoint,
#   b = right triangle endpoint
#   c = distribution mode
#   First, exclude situations which are impossible with the function definition
#   Next, define the value of the function on the various intervals
#
################################################################################

dtriangle <- function(x, a=0, b=1, c=(a+b)/2) 
{
  x1 <- length(x)
  a1 <- length(a)
  b1 <- length(b)
  c1 <- length(c)

  dTest <- function(X)
  {
    xx <- 1
    aa <- 2
    bb <- 3
    cc <- 4
    if (any(is.na(X))) # is.na is TRUE for NA, NaN, and FALSE
    { 
      if (any(is.nan(X))) return(NaN) # to conform to qunif
      else return(NA) # to conform to qunif
    } else if (X[aa] > X[cc] | X[bb] < X[cc] | (X[aa]==X[cc] & X[bb]==X[cc]))
    {
      warning("values required to be  a <= c <= b (at least one strict inequality)")
      return(NaN) # to conform to behavior of qunif
    } else if (any(is.infinite(X[aa:cc]))){
      return(NaN)
    } else if (X[xx] < X[aa]) {
      return(0)
    } else if (X[xx] == X[aa] & X[aa] == X[cc])
    {
      return(2*(X[bb] - X[xx]) / (X[bb] - X[aa]) / (X[bb] - X[cc]))
    } else if (X[aa] != X[cc] & X[xx] < X[cc])
    {
      return(2*(X[xx] - X[aa]) / (X[bb] - X[aa]) / (X[cc] - X[aa]))
    } else if (X[cc] != X[bb] & X[xx] >= X[cc] & X[xx] <= X[bb])
    {
      return(2*(X[bb] - X[xx]) / (X[bb] - X[aa]) / (X[bb] - X[cc]))
    } else if (X[xx] == X[bb] & X[bb] == X[cc])
    {
      return(2*(X[xx] - X[aa]) / (X[bb] - X[aa]) / (X[cc] - X[aa]))
    } else if (X[xx] > X[bb])
    {
      return(0)
    } 
  }

  k <- max(x1, a1, b1, c1)
  if (k==1) return(dTest(c(x, a, b, c)))

  params <- matrix(nrow=k, ncol=4)
  tryCatch(
  {
    params[,1] <- x
    params[,2] <- a
    params[,3] <- b
    params[,4] <- c
  }, error = function(X) {
    stop(paste(" -- Argument Lengths: length of x = ", x1,
                ", a = ", a1, ", b = ", b1, ", c = ", c1, " -- ", X, sep=""))
  })

  return(apply(params, 1, dTest))
}

