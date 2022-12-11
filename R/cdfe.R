# Copyright Robert Carnell 2022

#' Triangle parameter estimates using a non-linear fit of the empirical CDF
#'
#' @param x the triangle distributed sample
#'
#' @return an object of class \code{nls}
#' @export
#'
#' @importFrom stats nls
#'
#' @examples
#' xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
#' cdfe <- triangle_cdfe(xtest)
#' print(cdfe)
#' summary(cdfe)
#' coef(cdfe)
#' if (isNamespaceLoaded("MASS")) confint(cdfe)
triangle_cdfe <- function(x)
{
  # x <- rtriangle(100, 0, 1, .3)
  n <- length(x)
  ecdf1 <- sapply(1:n, function(i) length(which(x < x[i]))) / n
  minx <- min(x)
  maxx <- max(x)
  rangex <- maxx - minx

  nls1 <- stats::nls(
    ecdf1 ~ ptriangle(x, a, b, c),
    start = c(a = minx - 0.5*rangex,
              b = maxx + 0.5*rangex,
              c = 3*mean(x) - minx - maxx),
    algorithm = "port",
    lower = c(minx - 2*rangex, maxx, minx),
    upper = c(minx, maxx + 2*rangex, maxx))
  return(nls1)
}
