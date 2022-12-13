# Copyright Robert Carnell 2022

#' Triangle parameter estimates using a non-linear fit of the empirical CDF
#'
#' @param x the triangle distributed sample
#' @param control an object created by \code{stats::nls.control}
#'
#' @return an object of class \code{nls}
#' @export
#'
#' @importFrom stats nls nls.control
#'
#' @examples
#' set.seed(10304)
#' xtest <- rtriangle(100, 1, 5, 2)
#' cdfe <- triangle_cdfe(xtest)
#' print(cdfe)
#' summary(cdfe)
#' coef(cdfe)
#' \dontrun{
#'   confint(cdfe)
#' }
triangle_cdfe <- function(x, control = stats::nls.control(maxiter = 100, warnOnly = TRUE))
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
    upper = c(minx, maxx + 2*rangex, maxx),
    control = control)
  return(nls1)
}
