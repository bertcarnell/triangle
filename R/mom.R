#' Triangle distribution method of moments estimate
#'
#' @param x triangle distribution sample
#'
#' @return a vector of the parameter estimates
#' @export
#'
#' @examples
#' set.seed(1204)
#' x <- rtriangle(20, 0, 2, 1.5)
#' triangle_mom(x)
triangle_mom <- function(x)
{
  ap <- min(x)
  bp <- max(x)
  cp <- 3*mean(x) - ap - bp
  return(c(ap, bp, cp))
}

