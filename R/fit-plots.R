# Copyright Robert Carnell 2022

#' Quantile-Quantile Plot for Triangle Distributed Data
#'
#' @param y the triangle distributed sample
#' @param a the theoretical distribution triangle minimum parameter
#' @param b the theoretical distribution triangle maximum parameter
#' @param c the theoretical distribution triangle mode parameter
#' @param main the plot title
#' @param xlab the x-axis label
#' @param ylab the y-axis label
#' @param ... other parameters passed to \code{qqplot}
#'
#' @return a list of x-y coordinates on the plot
#' @export
#'
#' @importFrom stats qqplot ppoints
#' @importFrom graphics abline
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' theta <- coef(triangle_mle(xtest))
#' qqtriangle(xtest, theta[1], theta[2], theta[3])
qqtriangle <- function(y, a, b, c, main = "Triangle Q-Q Plot",
                       xlab = "Theoretical Quantiles",
                       ylab = "Sample Quantiles",
                       ...)
{
  # y <- rtriangle(100, 0, 5, 2)
  leny <- length(y)
  h <- stats::qqplot(qtriangle(stats::ppoints(leny), a, b, c), y, main = main,
                     xlab = xlab, ylab = ylab, ...)
  graphics::abline(0, 1)
  return(invisible(h))
}


#' Compare multiple triangle distributions fits
#'
#' @param y the triangle distributed sample
#' @param cols the colors of the CDF-based estimates, the maximum likelihood estimates, and the method of moments estimates
#' @param main the plot title
#' @param ... other parameters passed to \code{plot.ecdf}
#'
#' @importFrom stats coef ecdf plot.ecdf
#' @importFrom stats4 coef
#' @importFrom graphics lines legend
#'
#' @export
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' compare_triangle_fit(xtest)
compare_triangle_fit <- function(y, cols = c("red", "blue", "green"), main = "Triangle Fit Comparison", ...)
{
  # y <- rtriangle(1000, 0, 1, .3)
  theta1 <- stats::coef(triangle_cdfe(y))
  theta2 <- stats4::coef(triangle_mle(y))
  theta3 <- triangle_mom(y)

  n <- 1000
  basex <- seq(min(y), max(y), length = n)
  stats::plot.ecdf(stats::ecdf(y), main = main, ...)
  graphics::lines(basex, ptriangle(basex, theta1[1], theta1[2], theta1[3]), col = cols[1])
  graphics::lines(basex, ptriangle(basex, theta2[1], theta2[2], theta2[3]), col = cols[2])
  graphics::lines(basex, ptriangle(basex, theta3[1], theta3[2], theta3[3]), col = cols[3])
  graphics::legend("topleft", legend = c("CDF estimates", "MLE", "MOM estimates", "Empirical CDF"),
                   col = c(cols, "black"), lty = 1, pch = c(NA, NA, NA, 19))
}
