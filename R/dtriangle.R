# Copyright 2018 Rob Carnell

#' The Triangle Distribution
#'
#' @description These functions provide information about the triangle
#' distribution on the interval from \code{a} to \code{b} with a maximum at
#' \code{c}.  \code{dtriangle} gives the density, \code{ptriangle} gives the
#' distribution function, \code{qtriangle} gives the quantile function, and
#' \code{rtriangle} generates \code{n} random deviates.
#'
#' @details   All probabilities are lower tailed probabilities.
#' \code{a}, \code{b}, and \code{c} may be appropriate length vectors except in
#' the case of \code{rtriangle}.  \code{rtriangle} is derived from a draw from
#' \code{runif}. The triangle distribution has density:
#' \deqn{f(x) = \frac{2(x-a)}{(b-a)(c-a)}}{f(x) = 2(x-a) / [(b-a)(c-a)]}
#' for \eqn{a \le x < c}{a <= x < c}.
#' \deqn{f(x) = \frac{2(b-x)}{(b-a)(b-c)}}{f(x) = 2(b-x) / [(b-a)(b-c)]}
#' for \eqn{c \le x \le b}{c <= x <= b}.
#' \eqn{f(x) = 0} elsewhere.
#' The mean and variance are:
#' \deqn{E(x) = \frac{(a + b + c)}{3}}{E(x) = (a + b + c) / 3}
#' \deqn{V(x) = \frac{1}{18}(a^2 + b^2 + c^2 - ab - ac - bc)}{V(x) = (a^2 + b^2 + c^2 - ab - ac - bc) / 18}
#'
#' @param x,q vector of quantiles.
#' @param a lower limit of the distribution.
#' @param b upper limit of the distribution.
#' @param c mode of the distribution.
#' @param p vector of probabilities.
#' @param n number of observations. If \code{length(n) > 1}, the length is taken to be the number required.
#'
#' @return \code{dtriangle} gives the density, \code{ptriangle} gives the
#' distribution function, \code{qtriangle} gives the quantile function, and
#' \code{rtriangle} generates random deviates.  Invalid arguments will result
#' in return value \code{NaN} or \code{NA}.
#' @references
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}.  Wadsworth & Brooks/Cole.
#' @seealso
#' \code{\link{.Random.seed}} about random number generation,
#' \code{\link{runif}}, etc for other distributions.
#' @keywords distribution
#'
#' @name triangle
#' @export
#'
#' @examples
#' ## view the distribution
#' tri <- rtriangle(100000, 1, 5, 3)
#' hist(tri, breaks=100, main="Triangle Distribution", xlab="x")
#' mean(tri) # 1/3*(1 + 5 + 3) = 3
#' var(tri)  # 1/18*(1^2 + 3^2 + 5^2 - 1*5 - 1*3 - 5*3) = 0.666667
#' dtriangle(0.5, 0, 1, 0.5) # 2/(b-a) = 2
#' qtriangle(ptriangle(0.7)) # 0.7
dtriangle <- function(x, a=0, b=1, c=(a + b)/2)
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
      if (any(is.nan(X))) return(NaN) # to conform to dunif
      else return(NA) # to conform to qunif
    } else if (X[aa] > X[cc] | X[bb] < X[cc] | (X[aa]==X[cc] & X[bb]==X[cc]))
    {
      warning("values required to be  a <= c <= b (at least one strict inequality)")
      return(NaN) # to conform to behavior of dunif
    } else if (any(is.infinite(X[aa:cc])))
    {
      # If the statistics are in the right order, then return 0 to be consistent with dunif
      return(0)
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

