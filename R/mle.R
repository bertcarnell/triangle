# Copyright Robert Carnell 2022

#' Log of the maximization function.  The natural log of equation 1.43
#'
#' @references Samuel Kotz and Johan Rene van Dorp. Beyond Beta \doi{10.1142/5720}
#'
#' @noRd
#'
#' @param z the set of order statistics (sorted sample)
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param r the order statistic at which M is to be calculated \code{1 <= r <= length(z)}
#' @param debug if \code{TRUE} then \code{logM} will check the input parameters
#'
#' @return The \code{log(M)} from equation 1.43
#'
#' @examples
#' set.seed(103)
#' logM(rtriangle(10, 0, 1, 0.5), 0, 1, 3)
logM <- function(z, a, b, r, debug = FALSE)
{
  s <- length(z)
  if (debug)
  {
    if (length(r) != 1) stop("r must be a scalar")
    if (length(a) != 1) stop("a must be a scalar")
    if (length(b) != 1) stop("b must be a scalar")
    if (r > s | r < 1) stop("r must be on [1, length(z)]")
    if (a >= b) stop("a < b")
    if (a > min(z)) stop("a <= min(z)")
    if (b < max(z)) stop("b >= max(z)")
    if (!(all(order(z) == 1:s))) stop("z must be sorted")
  }
  if (r > 1 & r < s)
  {
    #return(prod((z[1:(rk-1)] - ak) / (z[rk] - ak)) * prod((bk - z[(rk+1):s]) / (bk - z[rk])))
    return(sum(log(z[1:(r-1)] - a)) - (r-1)*log(z[r] - a) + sum(log(b - z[(r+1):s])) - (s-r)*log(b - z[r]))
  } else if (r == 1)
  {
    #return(prod((bk - z[(rk+1):s]) / (bk - z[rk])))
    return(sum(log(b - z[(r+1):s])) - (s-r)*log(b - z[r]))
  } else if (r == s)
  {
    #return(prod((z[1:(rk-1)] - ak) / (z[rk] - ak)))
    return(sum(log(z[1:(r-1)] - a)) - (s-1)*log(z[r] - a))
  }
}

#' The order statistic which is the estimate of the mode of the triangle distribution
#' for a given \code{a} and \code{b} from equation 1.42
#'
#' @references Samuel Kotz and Johan Rene van Dorp. Beyond Beta \doi{10.1142/5720}
#'
#' @noRd
#'
#' @param z the set of order statistics (sorted sample)
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param debug if \code{TRUE} then the function will check the input parameters
#'
#' @return the order statistic number
#'
#' @examples
#' set.seed(40039)
#' rhat(rtriangle(10, 0, 1, 0.5), 0, 1)
rhat <- function(z, a, b, debug = FALSE)
{
  s <- length(z)
  if (debug)
  {
    if (length(a) != 1) stop("a must be a scalar")
    if (length(b) != 1) stop("b must be a scalar")
    if (a >= b) stop("a < b")
    if (a > min(z)) stop("a <= min(z)")
    if (b < max(z)) stop("b >= max(z)")
    if (!(all(order(z) == 1:s))) stop("z must be sorted")
  }
  res <- sapply(1:s, function(r) logM(z, a, b, r, debug))
  return(which.max(res))
}

#' Maximum likelihood estimate of c given a and b
#'
#' @noRd
#'
#' @param x sample from a triangle distribution
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param debug if \code{TRUE} then the function will check the input parameters
#'
#' @return a list containing the estimate of \code{c} and \code{r}
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' triangle_mle_c_given_ab(xtest, 0, 1)$c_hat
triangle_mle_c_given_ab <- function(x, a, b, debug = FALSE)
{
  if (debug)
  {
    if (length(a) != 1) stop("a must be a scalar")
    if (length(b) != 1) stop("b must be a scalar")
    if (a >= b) stop("a < b")
    if (a > min(x)) stop("a must be <= min(x)")
    if (b < max(x)) stop("b must be >= max(x)")
  }
  sx <- sort(x)
  r <- rhat(sx, a, b, debug)
  c_hat <- sx[r]
  if (debug)
  {
    stopifnot(a <= c_hat & c_hat <= b)
  }
  return(list(c_hat = c_hat, r_hat = r))
}

#' Negative log likelihood of a sample given a, b, and c
#'
#' @noRd
#'
#' @param x sample from a triangle distribution
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param c the mode of the triangle distribution
#' @param debug if \code{TRUE} then the function will check the input parameters
#'
#' @return the negative log likelihood
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' nLL_triangle(xtest, 0, 1, 0.3)
nLL_triangle <- function(x, a, b, c, debug = FALSE)
{
  n <- length(x)
  if (debug)
  {
    if (length(a) != 1) stop("a must be a scalar")
    if (length(b) != 1) stop("b must be a scalar")
    if (a >= b) stop("a < b")
    if (a > min(x)) stop(paste0("a (", a, ") <= min(x) (", min(x), ")"))
    if (b < max(x)) stop("b >= max(x)")
  }
  # a_hat = min(x) when a = c < b
  if (a == min(x) & a == c) {
    ret <- -n*log(2) + n*log(b-a) + n*log(b-c) - sum(log(b - x))
  } else if (b == max(x) & b == c) {
    ret <- -n*log(2) + n*log(b-a) + n*log(c-a) - sum(log(x - a))
  } else if (a == min(x) | b == max(x)) {
    ret <- Inf
  } else {
    ind1 <- which(x < c)
    ind2 <- which(x >= c)
    n1 <- length(ind1)
    n2 <- length(ind2)
    if (n1 > 0 & n2 > 0)
    {
      ret <- -n*log(2) + n*log(b-a) + n1*log(c-a) + n2*log(b-c) - sum(log(x[ind1] - a)) - sum(log(b - x[ind2]))
    } else if (n1 == 0)
    {
      ret <- -n*log(2) + n*log(b-a) + n*log(b-c) - sum(log(b - x[ind2]))
    } else if (n2 == 0)
    {
      ret <- -n*log(2) + n*log(b-a) + n*log(c-a) - sum(log(x[ind1] - a))
    }
  }
  return(ret)
}

#' Gradient of the Negative log likelihood of a sample given a fixed c
#'
#' @noRd
#'
#' @param x sample from a triangle distribution
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param c the mode of the triangle distribution
#' @param debug if \code{TRUE} then the function will check the input parameters
#'
#' @return the gradient of the negative log likelihood with components for \code{d/da} and \code{d/db}
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' gradient_nLL_triangle_given_c(xtest, 0, 1, 0.3)
gradient_nLL_triangle_given_c <- function(x, a, b, c, debug = FALSE)
{
  if (debug)
  {
    if (length(a) != 1) stop("a must be a scalar")
    if (length(b) != 1) stop("b must be a scalar")
    if (a >= b) stop("a < b")
    if (a > min(x)) stop(paste0("a (", a, ") <= min(x) (", min(x), ")"))
    if (b < max(x)) stop("b >= max(x)")
  }
  n <- length(x)
  ind1 <- which(x < c)
  ind2 <- which(x >= c)
  n1 <- length(ind1)
  n2 <- length(ind2)
  if (n1 > 0 & n2 > 0)
  {
    return(c(-n/(b - a) - ifelse(c > a, n1/(c - a), 0) + sum(ifelse(x[ind1] > a, 1/(x[ind1] - a), 0)),
             n/(b - a) + ifelse(b > c, n2/(b - c), 0) - sum(ifelse(x[ind2] < b, 1/(b - x[ind2]), 0))))
  } else if (n1 == 0)
  {
    return(c(-n/(b - a),
             n/(b - a) + ifelse(b > c, n2/(b - c), 0) - sum(ifelse(x[ind2] < b, 1/(b - x[ind2]), 0))))
  } else if (n2 == 0)
  {
    return(c(-n/(b - a) - ifelse(c > a, n1/(c - a), 0) + sum(ifelse(x[ind1] > a, 1/(x[ind1] - a), 0)),
             n/(b - a)))
  } else
  {
    stop("Unexpected result in gradient")
  }
}

#' Hessian of the Negative log likelihood of a sample given a fixed c
#'
#' @noRd
#'
#' @param x sample from a triangle distribution
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param c the mode of the triangle distribution
#' @param debug if \code{TRUE} then the function will check the input parameters
#'
#' @return the hessian of the negative log likelihood with respect to a and b
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' hessian_nLL_triangle_given_c(xtest, 0, 1, 0.3)
hessian_nLL_triangle_given_c <- function(x, a, b, c, debug = FALSE)
{
  if (debug)
  {
    if (a >= b) stop("a must be less than b")
  }
  n <- length(x)
  ind1 <- which(x < c)
  ind2 <- which(x >= c)
  n1 <- length(ind1)
  n2 <- length(ind2)
  dadb <- n / (b - a)^2
  if (n1 > 0 & n2 > 0)
  {
    da2 <- -n/(b - a)^2 - ifelse(c > a, n1/(c - a)^2, 0) + sum(ifelse(x[ind1] > a, 1/(x[ind1] - a)^2, 0))
    db2 <- -n/(b - a)^2 - ifelse(b > c, n2/(b - c)^2, 0) + sum(ifelse(x[ind2] < b, 1/(b - x[ind2])^2, 0))
  } else if (n1 == 0)
  {
    da2 <- -n/(b - a)^2
    db2 <- -n/(b - a)^2 - ifelse(b > c, n2/(b - c)^2, 0) + sum(ifelse(x[ind2] < b, 1/(b - x[ind2])^2, 0))
  } else if (n2 == 0)
  {
    da2 <- -n/(b - a)^2 - ifelse(c > a, n1/(c - a)^2, 0) + sum(ifelse(x[ind1] > a, 1/(x[ind1] - a)^2, 0))
    db2 <- -n/(b - a)^2
  } else
  {
    stop("Unexpected result in hessian")
  }
  return(matrix(c(da2, dadb, dadb, db2), nrow = 2, dimnames = list(c("a","b"), c("a","b"))))
}


#' Maximum likelihood estimate of a and b given a fixed c
#'
#' @noRd
#'
#' @param x sample from a triangle distribution
#' @param c the mode of the triangle distribution
#' @param debug if \code{TRUE} then the function will check the input parameters
#' @param start starting values for a and b
#' @param lower lower bounds for a and b
#' @param upper upper bounds for a and b
#'
#' @importFrom stats optim
#'
#' @return a list containing a, b, the results of stats::optim, and the analytic hessian
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' triangle_mle_ab_given_c(xtest, 0.3)
triangle_mle_ab_given_c <- function(x, c, debug = FALSE, start = NA, lower = NA, upper = NA)
{
  if (debug)
  {
    if (length(c) != 1) stop("c must be a scalar")
    if (c > max(x)) stop("c <= min(x)")
    if (c < min(x)) stop("c >= max(x)")
  }
  # x <- triangle::rtriangle(100, 0.001, 0.005, 0.004)
  nLL <- function(p, x, c, debug)
  {
    nLL_triangle(x, p[1], p[2], c, debug)
  }

  g_nLL <- function(p, x, c, debug)
  {
    gradient_nLL_triangle_given_c(x, p[1], p[2], c, debug)
  }

  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  rangex <- maxx - minx
  if (any(is.na(start)))
  {
    # start at method of moments estimates, but a little wider
    start <- c(minx - 0.05*rangex,
               maxx + 0.05*rangex)
  }
  if (any(is.na(lower)) | any(is.na(upper)))
  {
    lower <- c(minx - 2*rangex, # lower end of a can be low
               maxx + abs(.Machine$double.eps*maxx)) # lower end of b can be just above max(x)
    upper <- c(minx - abs(.Machine$double.eps*minx),
               maxx + 2*rangex)
  }
  mle_ab <- stats::optim(par = start, fn = nLL, gr = g_nLL, x = x, c = c, debug = debug,
                  method = "L-BFGS-B", lower = lower, upper = upper,
                  hessian = FALSE)
  return(list(a = mle_ab$par[1],
              b = mle_ab$par[2],
              optim = mle_ab,
              hessian_ab = hessian_nLL_triangle_given_c(x, mle_ab$par[1], mle_ab$par[2], c, debug)))
}

#' Density of the rth order statistic
#'
#' @noRd
#'
#' @param x The value of the rth order statistic
#' @param n number of order statistics \code{n > 0}
#' @param r the order statistic number \code{0 < r <= n}
#' @param a the minimum support of the triangle distribution \code{a < b}
#' @param b the maximum support of the triangle distribution
#' @param c the mode of the triangle distribution
#'
#' @return the density
#'
#' @examples
#' integrate(f_rth_order_stat, lower = 0, upper = 1, n = 10, r = 3, a = 0, b = 1, c = 0.5)
f_rth_order_stat <- function(x, n, r, a, b, c)
{
  ret <- exp(log(r) + lchoose(n, r) + log(dtriangle(x, a, b, c)) + (r-1) * log(ptriangle(x, a, b, c)) + (n-r) * log(1 - ptriangle(x, a, b, c)))
  # need to trap case when ptriangle returns 1 and r = n which causes an NaN (o * Inf)
  if (any(r == n))
  {
    ind <- which(r == n & x >= b)
    ret[ind] <- 0
  }
  return(ret)
}

#' Expected value of the rth order statistic
#'
#' @noRd
#'
#' @param n number of order statistics
#' @param r the order statistic number
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param c the mode of the triangle distribution
#'
#' @return the expected value
#'
#' @examples
#' mean_rth_order_stat(10, 5, 0, 1, 0.5)
mean_rth_order_stat <- function(n, r, a, b, c)
{
  return(mean_rth_order_stat_numeric(n, r, a, b, c))
}

#' Expected value of the rth order statistic using base precision
#'
#' @noRd
#'
#' @param n number of order statistics
#' @param r the order statistic number
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param c the mode of the triangle distribution
#'
#' @return the expected value
#'
#' @examples
#' mean_rth_order_stat_base(10, 5, 0, 1, 0.5)
mean_rth_order_stat_base <- function(n, r, a, b, c)
{
  coefs1 <- sapply(0:(n-r), function(k) {
    choose(n-r, k) * (b-a)^(k-n) * (c-a)^(n-k) * (-1)^(n-r-k) *
      (c/(n-k) - (c-a)/(n-k)/(2*n-2*k+1))
  })

  coefs2 <- sapply(0:(r-1), function(k) {
    choose(r-1, k) * (b-a)^(k-n) * (c-b)^(n-k) *
      (-c/(n-k) + (c-b)/(n-k)/(2*n-2*k+1))
  })

  return(ifelse(c > a, r*choose(n, r)*sum(coefs1), 0) +
           ifelse(c < b, r*choose(n, r)*(-1)^(n-r)*sum(coefs2), 0))
}

#' Expected value of the rth order statistic using multiple precision
#'
#' @noRd
#'
#' @param n number of order statistics
#' @param r the order statistic number
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param c the mode of the triangle distribution
#'
#' @return the expected value
#'
#' @examples
#' if (requireNamespace("Rmpfr", quietly = TRUE)) mean_rth_order_stat_rmpfr(10, 5, 0, 1, 0.5)
mean_rth_order_stat_rmpfr <- function(n, r, a, b, c)
{
  if (!requireNamespace("Rmpfr", quietly = TRUE))
    stop("Rmpfr is required for this function")

  calc_prec <- 2^8
  n <- as.integer(n)
  r <- as.integer(r)
  a <- Rmpfr::mpfr(a, calc_prec)
  b <- Rmpfr::mpfr(b, calc_prec)
  c <- Rmpfr::mpfr(c, calc_prec)

  f1 <- function(k) {
    (b-a)^(k-n) * (c-a)^(n-k) * (c/(n-k) - (c-a)/(n-k)/(2*n-2*k+1))
  }
  coefs1 <- Rmpfr::sumBinomMpfr(n-r, f1, n0 = 0, alternating = TRUE, precBits = calc_prec)

  f2 <- function(k) {
    (b-a)^(k-n) * (c-b)^(n-k) * (-c/(n-k) + (c-b)/(n-k)/(2*n-2*k+1))
  }
  coefs2 <- Rmpfr::sumBinomMpfr(r-1, f2, n0 = 0, alternating = FALSE, precBits = calc_prec)

  if (c == a) {
    tot <- r * Rmpfr::chooseMpfr(n, r) * Rmpfr::mpfr((-1)^(n-r), calc_prec) * coefs2
  } else if (c == b) {
    tot <- r * Rmpfr::chooseMpfr(n, r) * coefs1
  } else {
    tot <- r * Rmpfr::chooseMpfr(n, r) *
      (coefs1 + Rmpfr::mpfr((-1)^(n-r), 2^8) * coefs2)
  }

  return(as.numeric(tot))
}

#' Expected value of the rth order statistic using numeric integration
#'
#' @noRd
#'
#' @param n number of order statistics
#' @param r the order statistic number
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param c the mode of the triangle distribution
#'
#' @return the expected value
#'
#' @importFrom stats integrate
#' @examples
#' mean_rth_order_stat_numeric(10, 5, 0, 1, 0.5)
mean_rth_order_stat_numeric <- function(n, r, a, b, c)
{
  integrand <- function(x, n, r, a, b, c) {x * f_rth_order_stat(x, n, r, a, b, c)}
  stats::integrate(integrand, lower = a, upper = b, n = n, r = r, a = a, b = b, c = c)$value
}

#' Variance of the rth order statistic
#'
#' @noRd
#'
#' @param n number of order statistics
#' @param r the order statistic number
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param c the mode of the triangle distribution
#'
#' @return the variance
#'
#' @examples
#' variance_rth_order_stat(10, 5, 0, 1, 0.5)
variance_rth_order_stat <- function(n, r, a, b, c)
{
  variance_rth_order_stat_numeric(n, r, a, b, c)
}

#' Variance of the rth order statistic using base summation
#'
#' @noRd
#'
#' @param n number of order statistics
#' @param r the order statistic number
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param c the mode of the triangle distribution
#'
#' @return the variance
#'
#' @examples
#' variance_rth_order_stat_base(10, 5, 0, 1, 0.5)
variance_rth_order_stat_base <- function(n, r, a, b, c)
{
  coefs1 <- sapply(0:(n-r), function(k) {
    choose(n-r, k) * (b-a)^(k-n) * (c-a)^(n-k) * (-1)^(n-r-k) / (n-k) *
      (c^2 - 2*c*(c-a)/(2*n-2*k+1) + 2*(c-a)^2/(2*n-2*k+1)/(2*n-2*k+2))
  })
  coefs2 <- sapply(0:(r-1), function(k) {
    choose(r-1, k) * (b-a)^(k-n) * (c-b)^(n-k)/(n-k) *
      (-c^2 + 2*c*(c-b)/(2*n-2*k+1) - 2*(c-b)^2/(2*n-2*k+1)/(2*n-2*k+2))
  })
  m <- mean_rth_order_stat_base(n, r, a, b, c)
  return(ifelse(c > a, r*choose(n, r)*sum(coefs1), 0) +
           ifelse(c < b, r*choose(n, r)*(-1)^(n-r)*sum(coefs2), 0) - m^2)
}

#' Variance of the rth order statistic using multi-precision computation
#'
#' @noRd
#'
#' @param n number of order statistics
#' @param r the order statistic number
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param c the mode of the triangle distribution
#'
#' @return the variance
#'
#' @examples
#' if (requireNamespace("Rmpfr")) variance_rth_order_stat_rmpfr(10, 5, 0, 1, 0.5)
variance_rth_order_stat_rmpfr <- function(n, r, a, b, c)
{
  if (!requireNamespace("Rmpfr", quietly = TRUE))
    stop("Rmpfr is required for this function")

  calc_prec <- 2^8
  n <- as.integer(n)
  r <- as.integer(r)
  a <- Rmpfr::mpfr(a, calc_prec)
  b <- Rmpfr::mpfr(b, calc_prec)
  c <- Rmpfr::mpfr(c, calc_prec)

  f1 <- function(k) {
    (b-a)^(k-n) * (c-a)^(n-k) / (n-k) * (c^2 - 2*c*(c-a)/(2*n-2*k+1) + 2*(c-a)^2/(2*n-2*k+1)/(2*n-2*k+2))
  }
  coefs1 <- Rmpfr::sumBinomMpfr(n-r, f1, n0 = 0, alternating = TRUE, precBits = calc_prec)

  f2 <- function(k) {
    (b-a)^(k-n) * (c-b)^(n-k) / (n-k) * (-c^2 + 2*c*(c-b)/(2*n-2*k+1) - 2*(c-b)^2/(2*n-2*k+1)/(2*n-2*k+2))
  }
  coefs2 <- Rmpfr::sumBinomMpfr(r-1, f2, n0 = 0, alternating = FALSE, precBits = calc_prec)

  m <- mean_rth_order_stat_rmpfr(n, r, a, b, c)

  if (c == a) {
    tot <- r * Rmpfr::chooseMpfr(n, r) * Rmpfr::mpfr((-1)^(n-r), calc_prec) * coefs2 - m^2
  } else if (c == b) {
    tot <- r * Rmpfr::chooseMpfr(n, r) * coefs1 - m^2
  } else {
    tot <- r * Rmpfr::chooseMpfr(n, r) *
      (coefs1 + Rmpfr::mpfr((-1)^(n-r), 2^8) * coefs2) - m^2
  }

  return(as.numeric(tot))
}

#' Variance of the rth order statistic using numeric integration
#'
#' @noRd
#'
#' @param n number of order statistics
#' @param r the order statistic number
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param c the mode of the triangle distribution
#'
#' @return the variance
#'
#' @importFrom stats integrate
#' @examples
#' variance_rth_order_stat_numeric(10, 5, 0, 1, 0.5)
variance_rth_order_stat_numeric <- function(n, r, a, b, c)
{
  integrand <- function(x, n, r, a, b, c) {x * x * f_rth_order_stat(x, n, r, a, b, c)}
  E_x2 <- stats::integrate(integrand, lower = a, upper = b, n = n, r = r, a = a, b = b, c = c)
  E_x <- mean_rth_order_stat_numeric(n, r, a, b, c)
  E_x2$value - E_x * E_x
}

iterative_triangle_mle <- function(x, debug = FALSE, maxiter = 100)
{
  sx <- sort(x)
  n <- length(x)
  # if c = sx[1] then a will optimize to c = a = sx[1]
  # if c = sx[n] then b will optimize to c = b = sx[n]
  # start at MOM estimate
  mom <- triangle_mom(x, na.rm = TRUE, type = 2)
  # find x closest to mom["c"]
  rhat1 <- which.min(abs(sx - mom["c"]))
  mle_c0 <- sx[rhat1]
  if (rhat1 == 1) {
    mle_ab <- triangle_mle_ab_given_c(x, c = mle_c0, debug = debug, start = c(mle_c0, mom["b"]))
  } else if (rhat1 == n) {
    mle_ab <- triangle_mle_ab_given_c(x, c = mle_c0, debug = debug, start = c(mom["a"], mle_c0))
  } else {
    mle_ab <- triangle_mle_ab_given_c(x, c = mle_c0, debug = debug, start = c(mom["a"], mom["b"]))
  }

  # then determine c given a and b
  mle_c1 <- triangle_mle_c_given_ab(x, mle_ab$a, mle_ab$b, debug = debug)

  # then optimize a and b given c
  mle_ab <- triangle_mle_ab_given_c(x, c = mle_c1$c_hat, debug = debug, start = c(mle_ab$a, mle_ab$b))

  # then check to see if c has changed
  mle_c2 <- triangle_mle_c_given_ab(x, mle_ab$a, mle_ab$b, debug = debug)

  # as long as the estimate of c is changing, keep going up to max iter
  count <- 1
  while (mle_c1$c_hat != mle_c2$c_hat & count < maxiter)
  {
    mle_c1 <- mle_c2

    mle_ab <- triangle_mle_ab_given_c(x, c = mle_c1$c_hat, debug = debug, start = c(mle_ab$a, mle_ab$b))

    mle_c2 <- triangle_mle_c_given_ab(x, mle_ab$a, mle_ab$b, debug = debug)

    count <- count + 1
  }

  if (count >= maxiter)
  {
    warning("Maximum iterations reached in triangle_mle without convergence on c")
  } else if (debug)
  {
    cat("\n", count, " iterations reached\n")
  }
  return(list(a=unname(mle_ab$a), b=unname(mle_ab$b),
              c=mle_c2$c_hat, r=mle_c2$r_hat,
              hessian_ab=mle_ab$hessian_ab, optim=mle_ab$optim))
}

enumerative_triangle_mle <- function(x, debug = FALSE)
{
  #x <- xtest_small
  sx <- sort(x)
  n <- length(x)
  mom <- triangle_mom(x, na.rm = TRUE, type = 2)
  res <- sapply(sx, function(ci) {
    mle_ab <- triangle_mle_ab_given_c(x, c = ci, debug = FALSE,
                                      start = c(min(mom["a"], sx[1]),
                                                max(mom["b"], sx[n])))
    nLL <- nLL_triangle(x, mle_ab$a, mle_ab$b, ci, debug = FALSE)
    return(list(a=mle_ab$a, b=mle_ab$b, c=ci, nLL = nLL,
                hessian_ab = mle_ab$hessian_ab,
                optim = mle_ab$optim))
  })
  ind <- which.min(res["nLL",])

  return(list(a=res[,ind]$a, b=res[,ind]$b,
              c=res[,ind]$c, r=ind,
              hessian_ab=res[,ind]$hessian_ab,
              optim=res[,ind]$optim))
}

#' Maximum likelihood estimate of the triangle distribution parameters
#'
#' @references Samuel Kotz and Johan Rene van Dorp. Beyond Beta \doi{10.1142/5720}
#'
#' @param x sample from a triangle distribution
#' @param debug if \code{TRUE} then the function will check the input parameters and print calculation information
#' @param maxiter the maximum number of cycles of optimization between maximizing \code{a} and \code{b} given \code{c}
#' and maximizing \code{c} given \code{a} and \code{b}
#' @param boot_var should the variance be computed with a bootstrap sample?
#' @param boot_rep The number of bootstrap replications
#'
#' @return an object of S3 class \code{triangle_mle} containing a list with the call, coefficients,
#' variance co-variance matrix, minimum negative log likelihood, details of the optimization
#' number of observations, and the sample
#' @export
#'
#' @importFrom boot boot
#' @importFrom stats var
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' triangle_mle(xtest)
#'
#' xtest <- rtriangle(20, 1, 5, 3.5)
#' triangle_mle(xtest)
triangle_mle <- function(x, debug = FALSE, maxiter = 100, boot_var = FALSE, boot_rep = 500)
{
  # x <- triangle::rtriangle(100, 0, 1, 0.5)
  # debug <- TRUE
  # maxiter <- 100
  # boot_var <- TRUE
  # boot_rep <- 100
  my_call <- match.call()
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  rangex <- maxx - minx

  if (length(x) <= 20) {
    mle <- enumerative_triangle_mle(x, debug = debug)
  } else {
    mle <- iterative_triangle_mle(x, debug = debug, maxiter = maxiter)
  }

  # check to see if there is an NA in the coefficients
  if (any(is.na(mle))) {
    print(x)
    print(minx)
    print(maxx)
    print(rangex)
    stop("NA coefficient")
  }

  var_chat <- variance_rth_order_stat(length(x), mle$r, mle$a, mle$b, mle$c)

  if (any(var_chat < 0) | debug) {
    if (any(var_chat < 0)) {
      cat("\nNegative Varince in c hat\n")
    }
    cat("n=", length(x), "\n")
    cat("r=", mle$r, "\n")
    cat("a=", mle$a, "\n")
    cat("b=", mle$b, "\n")
    cat("c=", mle$c, "\n")
  }

  if (boot_var) {
    b <- boot::boot(x, statistic = function(d, i) {
      mle <- iterative_triangle_mle(d[i], debug = FALSE, maxiter = maxiter)
      return(c(mle$a, mle$b, mle$c))
    }, R = boot_rep)

    vcov_theta <- stats::var(b$t)
    dimnames(vcov_theta) <- list(c("a", "b", "c"), c("a", "b", "c"))
  } else {
    hess <- hessian_nLL_triangle_given_c(x, mle$a, mle$b, mle$c, debug = debug)
    # or hess <- mle$hessian_ab
    var_hess <- NA
    try({
      var_hess <- solve(hess)
    }, silent = TRUE)
    if (inherits(var_hess, "matrix") & all(diag(var_hess) >= 0)) {
      vcov_theta <- rbind(cbind(var_hess, c(0, 0)), c(0, 0, var_chat))
    } else {
      vcov_theta <- matrix(NA, nrow = 3, ncol = 3)
    }
    dimnames(vcov_theta) <- list(c("a", "b", "c"), c("a", "b", "c"))
  }

  structure(list(call = my_call,
                 coef = c(a = mle$a, b = mle$b, c = mle$c),
                 vcov = vcov_theta,
                 min = mle$optim$value,
                 details = mle$optim,
                 nobs = length(x),
                 x = x),
            class = "triangle_mle")
}

#' Maximum likelihood estimate of the standard triangle distribution mode
#'
#' @references Samuel Kotz and Johan Rene van Dorp. Beyond Beta \doi{10.1142/5720}
#'
#' @param x sample from a triangle distribution
#' @param debug if \code{TRUE} then the function will check the input parameters and print calculation information
#'
#' @return an object of S3 class \code{triangle_mle} containing a list with the call, coefficients,
#' variance co-variance matrix, minimum negative log likelihood,
#' number of observations, and the sample
#' @export
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' standard_triangle_mle(xtest)
#'
#' xtest <- rtriangle(20, 0, 1, 0.63)
#' standard_triangle_mle(xtest)
standard_triangle_mle <- function(x, debug = FALSE)
{
  my_call <- match.call()
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)

  if (minx < 0 | maxx > 1) stop("standard triangle requires all samples be between [0, 1]")

  # determine c at a = min(x), b = max(x)
  mle_c1 <- triangle_mle_c_given_ab(x, 0, 1, debug = debug)

  var_chat <- variance_rth_order_stat(length(x), mle_c1$r_hat, 0, 1, mle_c1$c_hat)

  structure(list(call = my_call,
                 coef = c(a = 0, b = 1, c = mle_c1$c_hat),
                 vcov = matrix(c(rep(0, 8), var_chat), nrow = 3, ncol = 3),
                 min = nLL_triangle(x, 0, 1, mle_c1$c_hat, debug),
                 details = NA,
                 nobs = length(x),
                 x = x),
            class = "triangle_mle")
}


