
#' Utility Methods for S3 class triangle_mle
#'
#' @param object class triangle_mle from a call to \code{triangle_mle()}
#' @param x the \code{triangle_mle} object
#' @param ... not used except for \code{print} (other arguments passed to \code{printCoefmat})
#'
#' @rdname mle-utils
#'
#' @return an object of class summary.mle
#' @method summary triangle_mle
#' @export
#'
#' @importClassesFrom stats4 summary.mle
#' @importFrom stats4 show
#' @importFrom methods new
#'
#' @examples
#' set.seed(1234)
#' x <- rtriangle(100, 0, 1, 0.5)
#' mle1 <- triangle_mle(x)
#' summary(mle1)
#' print(mle1)
#' coef(mle1)
#' logLik(mle1)
#' AIC(mle1)
#' BIC(mle1)
#' vcov(mle1)
#' \dontrun{
#'   prof <- profile(mle1)
#'   stats4::plot(prof)
#'   confint(mle1, 1:3, level = 0.95)
#' }
summary.triangle_mle <- function(object, ...)
{
  vars <- diag(object$vcov)
  # it is possible to have negative numbers in the vcov because of numerical precision
  vars[which(vars < 0)] <- NA
  cmat <- cbind(Estimate = object$coef, `Std. Error` = sqrt(vars))
  m2logL <- 2 * object$min
  methods::new("summary.mle", call = object$call, coef = cmat, m2logL = m2logL)
}

#' @rdname mle-utils
#' @return x invisibly
#' @export
#'
#' @importFrom stats printCoefmat
#' @importFrom stats4 coef vcov
print.triangle_mle <- function(x, ...)
{
  vars <- diag(stats4::vcov(x))
  # it is possible to have negative numbers in the vcov because of numerical precision
  vars[which(vars < 0)] <- NA
  cat("Triangle Maximum Likelihood Estimates")
  cat("\n\nCall: ", deparse(x$call), "\n")
  cat("\nEstimates:\n")
  cmat <- cbind(stats4::coef(x), sqrt(vars))
  colnames(cmat) <- c("Estimate", "Std.Err")
  stats::printCoefmat(cmat, ...)
  cat("\nConvergence Code: ", ifelse(all(is.na(x$details)), NA, x$details$convergence))
  cat("\n\t", ifelse(all(is.na(x$details)), "", x$details$message))
  invisible(x)
}

#' @rdname mle-utils
#' @return a vector of coefficients
#' @method coef triangle_mle
#' @export
coef.triangle_mle <- function(object, ...) object$coef

#' @rdname mle-utils
#' @return an object of class logLik
#' @export
logLik.triangle_mle <- function(object, ...)
{
  structure(-1*object$min,
            df = length(object$coef),
            class = "logLik")
}

#' @rdname mle-utils
#' @param k the penalty per parameter to be used; the default k = 2
#' @return the AIC
#' @export
AIC.triangle_mle <- function(object, ..., k=2)
{
  lls <- logLik.triangle_mle(object)
  -2 * as.numeric(lls) + k * attr(lls, "df")
}

#' @rdname mle-utils
#' @return the BIC
#' @importFrom stats BIC
#' @export
BIC.triangle_mle <- function(object, ...)
{
  lls <- logLik.triangle_mle(object)
  nobs <- object$nobs
  -2 * as.numeric(lls) + log(nobs) * attr(lls, "df")
}

#' @rdname mle-utils
#' @return the variance co-variance matrix
#' @method vcov triangle_mle
#' @export
vcov.triangle_mle <- function(object, ...)
{
  object$vcov
}

#' @rdname mle-utils
#' @return an object of class profile.mle
#' @param fitted an object of class triangle_mle
#' @export
#'
#' @importClassesFrom stats4 profile.mle
#' @importFrom methods new
#' @importFrom stats qchisq
profile.triangle_mle <- function(fitted, ...)
{
  #xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
  #fitted <- triangle_mle(xtest)
  #fitted <- standard_triangle_mle(xtest, debug = TRUE)
  alpha = 0.01
  zmax = sqrt(stats::qchisq(1-alpha, 1))

  p <- length(fitted$coef)
  stopifnot(p == 3)
  summ <- summary(fitted)
  std.err <- summ@coef[, "Std. Error"]
  B0 <- fitted$coef
  Pnames <- names(B0)

  maxa <- min(fitted$x) - .Machine$double.eps * min(fitted$x)
  minb <- max(fitted$x) + .Machine$double.eps * max(fitted$x)

  sox <- sort(fitted$x)
  r <- which(sox == B0[3])

  prof <- vector("list", length = 3)
  names(prof) <- Pnames

  calc_z <- function(a, b, c, mysign)
  {
    nLLi <- nLL_triangle(fitted$x, a, b, c)
    nLLbase <- nLL_triangle(fitted$x, B0[1], B0[2], B0[3])
    if (nLLi < nLLbase) return(0)
    ret <- mysign * sqrt(2 * (nLLi - nLLbase))
    return(ret)
  }

  # up
  abc <- list(a=B0[1], b=B0[2], c=B0[3])
  z <- list(a=0, b=0, c=0)
  for (j in 1:3)
  {
    # if any parameters have been fixed and given 0 variance
    #   set up a dummy return value with at least 4 points.
    if (std.err[j] == 0)
    {
      z[[j]] <- c(z[[j]], 1000, -1000, 2000)
      abc[[j]] <- c(abc[[j]],
                    ifelse(B0[j] == 0, B0[j] + 0.000001, B0[j] + B0[j]*0.000001),
                    ifelse(B0[j] == 0, B0[j] - 0.000001, B0[j] - B0[j]*0.000001),
                    ifelse(B0[j] == 0, B0[j] + 0.000002, B0[j] + B0[j]*0.000002))
      next
    }
    ri <- r
    zi <- 0
    abci <- B0[j]
    while (zi < zmax)
    {
      ri <- ri + 1
      if (j == 3 & ri > length(fitted$x))
        break
      abci <- ifelse(j == 1, (maxa + abci) / 2, ifelse(j == 2, abci + std.err[j], sox[ri]))
      zi <- ifelse(j == 1, calc_z(abci, B0[2], B0[3], 1),
                   ifelse(j == 2, calc_z(B0[1], abci, B0[3], 1),
                          calc_z(B0[1], B0[2], abci, 1)))
      z[[j]] <- c(z[[j]], zi)
      abc[[j]] <- c(abc[[j]], abci)
    }
    # down
    ri <- r
    zi <- 0
    abci <- B0[j]
    while (zi > -zmax)
    {
      ri <- ri - 1
      if (j == 3 & ri < 1)
        break
      abci <- ifelse(j == 1, abci - std.err[j], ifelse(j == 2, (minb + abci) / 2, sox[ri]))
      zi <- ifelse(j == 1, calc_z(abci, B0[2], B0[3], -1),
                   ifelse(j == 2, calc_z(B0[1], abci, B0[3], -1),
                          calc_z(B0[1], B0[2], abci, -1)))
      z[[j]] <- c(z[[j]], zi)
      abc[[j]] <- c(abc[[j]], abci)
    }
  }

  for (j in 1:3)
  {
    # eliminate duplicate z
    ind <- which(duplicated(z[[j]]))
    if (length(ind) > 0)
    {
      z[[j]] <- z[[j]][-ind]
      abc[[j]] <- abc[[j]][-ind]
    }

    par.vals.i <- matrix(rep(B0, each = length(z[[j]])), nrow = length(z[[j]]), ncol = 3, dimnames = list(NULL, Pnames))
    par.vals.i[,j] <- abc[[j]]

    ord <- order(z[[j]])
    prof[[j]] <- data.frame(z = z[[j]][ord])
    prof[[j]]$par.vals <- par.vals.i[ord, , drop = FALSE]
  }
  methods::new("profile.mle", profile = prof, summary = summ)
}

# Note that extra documentation about this being an S3 method (@exportS3Method and @method) was required to product the correct .Rd file
# @exportS3Method stats::confint

#' @rdname mle-utils
#' @param parm parameters
#' @param level confidence interval level
#' @return an object of class profile.mle
#' @method confint triangle_mle
#' @export
#' @importClassesFrom stats4 profile.mle
#' @importFrom stats4 confint
confint.triangle_mle <- function(object, parm, level = 0.95, ...)
{
  prof <- profile.triangle_mle(object)
  stats4::confint(prof, parm, level, ...)
}
