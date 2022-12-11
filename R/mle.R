# Copyright Robert Carnell 2022

#' Log of the maximization function.  The natural log of equation 1.43
#'
#' @references \url{https://www.worldscientific.com/doi/suppl/10.1142/5720/suppl_file/5720_chap1.pdf}
#'
#' @noRd
#'
#' @param z the set of order statistics (sorted sample)
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param r the order statistic at which M is to be calculated \code{1 <= r <= length(z)}
#' @param debug if \code{TRUE} then \code{logM} will check the input parameters
#'
#' @importFrom assertthat assert_that
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
    assertthat::assert_that(length(r) == 1, msg = "r must be a scalar")
    assertthat::assert_that(length(a) == 1, msg = "a must be a scalar")
    assertthat::assert_that(length(b) == 1, msg = "b must be a scalar")
    assertthat::assert_that(r <= s & r >= 1, msg = "r must be on [1, length(z)]")
    assertthat::assert_that(a < b, msg = "a < b")
    assertthat::assert_that(a <= min(z), msg = "a <= min(z)")
    assertthat::assert_that(b >= max(z), msg = "b >= max(z)")
    assertthat::assert_that(all(order(z) == 1:s), msg = "z must be sorted")
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
#' @references \url{https://www.worldscientific.com/doi/suppl/10.1142/5720/suppl_file/5720_chap1.pdf}
#'
#' @noRd
#'
#' @param z the set of order statistics (sorted sample)
#' @param a the minimum support of the triangle distribution \code{a < b}, \code{a <= min(z)}
#' @param b the maximum support of the triangle distribution \code{b >= max(z)}
#' @param debug if \code{TRUE} then the function will check the input parameters
#'
#' @importFrom assertthat assert_that
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
    assertthat::assert_that(length(a) == 1, msg = "a must be a scalar")
    assertthat::assert_that(length(b) == 1, msg = "b must be a scalar")
    assertthat::assert_that(a < b, msg = "a < b")
    assertthat::assert_that(a <= min(z), msg = "a <= min(z)")
    assertthat::assert_that(b >= max(z), msg = "b >= max(z)")
    assertthat::assert_that(all(order(z) == 1:s), msg = "z must be sorted")
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
#' @importFrom assertthat assert_that
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
    assertthat::assert_that(length(a) == 1, msg = "a must be a scalar")
    assertthat::assert_that(length(b) == 1, msg = "b must be a scalar")
    assertthat::assert_that(a < b, msg = "a < b")
    assertthat::assert_that(a <= min(x), msg = "a must be <= min(x)")
    assertthat::assert_that(b >= max(x), msg = "b must be >= max(x)")
  }
  sx <- sort(x)
  r <- rhat(sx, a, b, debug)
  c_hat <- sx[r]
  if (debug)
  {
    assertthat::assert_that(a <= c_hat & c_hat <= b)
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
#' @importFrom assertthat assert_that
#'
#' @return the negative log likelihood
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' nLL_triangle(xtest, 0, 1, 0.3)
nLL_triangle <- function(x, a, b, c, debug = FALSE)
{
  if (debug)
  {
    assertthat::assert_that(length(a) == 1, msg = "a must be a scalar")
    assertthat::assert_that(length(b) == 1, msg = "b must be a scalar")
    assertthat::assert_that(a < b, msg = "a < b")
    assertthat::assert_that(a <= min(x), msg = "a <= min(x)")
    assertthat::assert_that(b >= max(x), msg = "b >= max(x)")
  }
  n <- length(x)
  ind1 <- which(x < c)
  ind2 <- which(x >= c)
  n1 <- length(ind1)
  n2 <- length(ind2)
  if (n1 > 0 & n2 > 0)
  {
    ret <- -n*log(2) + n*log(b-a) + n1*log(c-a) + n2*log(b-c) - sum(log(x[ind1] - a)) - sum(log(b-x[ind2]))
  } else if (n1 == 0)
  {
    ret <- -n*log(2) + n*log(b-a) + n2*log(b-c) - sum(log(b-x[ind2]))
  } else if (n2 == 0)
  {
    ret <- -n*log(2) + n*log(b-a) + n1*log(c-a) - sum(log(x[ind1] - a))
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
#' @importFrom assertthat assert_that
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
    assertthat::assert_that(length(a) == 1, msg = "a must be a scalar")
    assertthat::assert_that(length(b) == 1, msg = "b must be a scalar")
    assertthat::assert_that(a < b, msg = "a < b")
    assertthat::assert_that(a <= min(x), msg = "a <= min(x)")
    assertthat::assert_that(b >= max(x), msg = "b >= max(x)")
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
#' @importFrom assertthat assert_that
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
    assertthat::assert_that(a < b, msg = "a must be less than b")
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
#' @importFrom assertthat assert_that
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
    assertthat::assert_that(length(c) == 1, msg = "c must be a scalar")
    assertthat::assert_that(c <= max(x), msg = "c <= min(x)")
    assertthat::assert_that(c >= min(x), msg = "c >= max(x)")
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
    start <- c(minx - 0.5*rangex,
               maxx + 0.5*rangex)
  }
  if (any(is.na(lower)) | any(is.na(upper)))
  {
    lower <- c(minx - 2*rangex, # lower end of a can be low
               maxx + .Machine$double.eps*maxx) # lower end of b can be just above max(x)
    upper <- c(minx - .Machine$double.eps*minx,
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
#' @importFrom stats integrate
#' @examples
#' mean_rth_order_stat(10, 5, 0, 1, 0.5)
mean_rth_order_stat <- function(n, r, a, b, c)
{
  integrand <- function(x, n, r, a, b, c) {x * f_rth_order_stat(x, n, r, a, b, c)}
  stats::integrate(integrand, lower = a, upper = b, n = n, r = r, a = a, b = b, c = c)
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
#' @return the expected value
#'
#' @importFrom stats integrate
#' @examples
#' variance_rth_order_stat(10, 5, 0, 1, 0.5)
variance_rth_order_stat <- function(n, r, a, b, c)
{
  integrand <- function(x, n, r, a, b, c) {x * x * f_rth_order_stat(x, n, r, a, b, c)}
  E_x2 <- stats::integrate(integrand, lower = a, upper = b, n = n, r = r, a = a, b = b, c = c)
  E_x <- mean_rth_order_stat(n, r, a, b, c)
  E_x2$value - E_x$value * E_x$value
}

#' Maximum likelihood estimate of the triangle distribution parameters
#'
#' @references \url{https://www.worldscientific.com/doi/suppl/10.1142/5720/suppl_file/5720_chap1.pdf}
#'
#' @param x sample from a triangle distribution
#' @param debug if \code{TRUE} then the function will check the input parameters
#' @param maxiter the maximum number of cycles of optimization between maximizing a and b given c
#' and maximizing c given a an b
#'
#' @return an object of S3 class \code{triangle_mle} containing a list with the call, coefficients,
#' variance co-variance matrix, minimum negative log likelihood, details of the optimization
#' number of observations, and the sample
#' @export
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' triangle_mle(xtest)
triangle_mle <- function(x, debug = FALSE, maxiter = 100)
{
  # x <- triangle::rtriangle(100, 0, 1, 0.5)
  # debug <- TRUE
  # maxiter <- 100
  my_call <- match.call()
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  rangex <- maxx - minx

  # first determine c at a = min(x), b = max(x)
  mle_c1 <- triangle_mle_c_given_ab(x, minx - 0.5*rangex, maxx + 0.5*rangex, debug = debug)

  # then optimize a and b given c
  mle_ab <- triangle_mle_ab_given_c(x, c = mle_c1$c_hat, debug = debug)

  # then check to see if c has changed
  mle_c2 <- triangle_mle_c_given_ab(x, mle_ab$a, mle_ab$b, debug = debug)

  # as long as the estimate of c is changing, keep going up to max iter
  count <- 1
  while (mle_c1$c_hat != mle_c2$c_hat & count < maxiter)
  {
    mle_c1 <- mle_c2

    mle_ab <- triangle_mle_ab_given_c(x, c = mle_c1$c_hat, debug = debug)

    mle_c2 <- triangle_mle_c_given_ab(x, mle_ab$a, mle_ab$b, debug = debug)

    count <- count + 1
  }

  var_chat <- variance_rth_order_stat(length(x), mle_c2$r_hat, mle_ab$a, mle_ab$b, mle_c2$c_hat)
  vcov <- rbind(cbind(solve(mle_ab$hessian_ab), c(0, 0)), c(0, 0, var_chat))
  dimnames(vcov) <- list(c("a", "b", "c"), c("a", "b", "c"))

  structure(list(call = my_call,
                 coef = c(a = mle_ab$a, b = mle_ab$b, c = mle_c2$c_hat),
                 vcov = vcov,
                 min = mle_ab$optim$value,
                 details = mle_ab$optim,
                 nobs = length(x),
                 x = x),
            class = "triangle_mle")
}

#' Maximum likelihood estimate of the standard triangle distribution mode
#'
#' @references \url{https://www.worldscientific.com/doi/suppl/10.1142/5720/suppl_file/5720_chap1.pdf}
#'
#' @param x sample from a triangle distribution
#' @param debug if \code{TRUE} then the function will check the input parameters
#'
#' @return an object of S3 class \code{triangle_mle} containing a list with the call, coefficients,
#' variance co-variance matrix, minimum negative log likelihood,
#' number of observations, and the sample
#' @export
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' standard_triangle_mle(xtest)
standard_triangle_mle <- function(x, debug = FALSE)
{
  my_call <- match.call()
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)

  assertthat::assert_that(minx >= 0 & maxx <= 1, msg = "standard triangle requires all samples be between [0, 1]")

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


