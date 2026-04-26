# Copyright 2024 Robert Carnell

#' Triangle distribution method of moments estimate
#'
#' @param x triangle distribution sample
#' @param type the type of method of moments.  Type 1 uses the min and max.  Type2
#' minimizes the distance from the calculated mean, variance, and skewness to the
#' sample mean, variance, and skewness
#'
#' @return a named vector of the parameter estimates
#' @export
#'
#' @examples
#' set.seed(1204)
#' x <- rtriangle(20, 0, 2, 1.5)
#' triangle_mom(x)
triangle_mom <- function(x, na.rm = FALSE, type = 1)
{
  ap <- min(x, na.rm = na.rm)
  bp <- max(x, na.rm = na.rm)
  mean_x <- mean(x, na.rm = na.rm)
  if (type == 1)
  {
    cp <- 3*mean_x - ap - bp
    return(c(a=ap, b=bp, c=cp))
  } else if (type == 2)
  {
    var_x <- var(x, na.rm = na.rm)
    skew_x <- .skew(x, na.rm = na.rm)
    opt_fn <- function(theta) {
      a <- theta[1]
      b <- theta[2]
      c <- theta[3]
      if (a >= b | c < a | c > b) return(.Machine$double.xmax)
      pred_mean <- (a + b + c) / 3
      pred_var <- (a^2 + b^2 + c^2 - a*b - a*c - b*c) / 18
      pred_skew <- sqrt(2) * (a + b - 2*c) * (2*a - b - c) * (a - 2*b + c) / 5 / (18*pred_var)^(3/2)
      return((pred_mean - mean_x)^2 + (pred_var - var_x)^2 + (pred_skew - skew_x)^2)
    }
    o <- optim(par = c(ap, bp, 3*mean_x - ap - bp), fn = opt_fn)
    if (o$convergence != 0) {
      warning("method of moments did not converge.  check attr(*, 'Optim')")
    }
    ret_value <- c(a=o$par[1], b=o$par[2], c=o$par[3])
    attr(ret_value, "optim") <- o
    return(ret_value)
  } else {
    stop("type not recognized")
  }
}

.skew <- function (x, na.rm = FALSE)
{
  if (any(is.na(x))) {
    if (na.rm) {
      x <- x[which(!is.na(x))]
    }
    else return(NA)
  }
  n <- length(x)
  x <- x - mean(x)
  return(sqrt(n) * sum(x^3)/(sum(x^2)^(3/2)))
}
