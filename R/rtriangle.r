# Copyright 2018 Rob Carnell

#' @include dtriangle.R
#' @rdname triangle
#' @importFrom stats runif
#' @export
rtriangle <- function(n=1, a=0, b=1, c=(a + b)/2){
  if (length(n) > 1) n <- length(n)
  if (n < 1 | is.na(n)) stop(paste("invalid argument: n =", n))
  n <- floor(n)
  if (any(is.na(c(a,b,c)))) return(rep(NaN, times = n)) # to match behavior of runif
  if (a > c | b < c) return(rep(NaN, times = n)) # to match behavior of runif
  if (any(is.infinite(c(a,b,c)))) return(rep(NaN, times = n))

  p <- runif(n)

  if (a != c) {
    # if a = c then i is always true
    i <- which((a + sqrt(p * (b - a)*(c - a))) <= c)
    j <- which((b - sqrt((1 - p) * (b - a) * (b - c))) > c)
  } else {
    i <- which((a + sqrt(p * (b - a)*(c - a))) < c)
    j <- which((b - sqrt((1 - p) * (b - a) * (b - c))) >= c)
  }
  if (length(i) != 0)
    p[i] <- a + sqrt(p[i] * (b - a) * (c - a))
  if (length(j) != 0)
    p[j] <- b - sqrt((1 - p[j]) * (b - a) * (b - c))

  return(p)
}
