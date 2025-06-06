% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mle.R
\name{triangle_mle}
\alias{triangle_mle}
\title{Maximum likelihood estimate of the triangle distribution parameters}
\usage{
triangle_mle(x, debug = FALSE, maxiter = 100, boot_var = FALSE, boot_rep = 500)
}
\arguments{
\item{x}{sample from a triangle distribution}

\item{debug}{if \code{TRUE} then the function will check the input parameters and print calculation information}

\item{maxiter}{the maximum number of cycles of optimization between maximizing \code{a} and \code{b} given \code{c}
and maximizing \code{c} given \code{a} and \code{b}}

\item{boot_var}{should the variance be computed with a boostrap sample?}

\item{boot_rep}{The number of boostrap replications}
}
\value{
an object of S3 class \code{triangle_mle} containing a list with the call, coefficients,
variance co-variance matrix, minimum negative log likelihood, details of the optimization
number of observations, and the sample
}
\description{
Maximum likelihood estimate of the triangle distribution parameters
}
\examples{
xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
triangle_mle(xtest)

xtest <- rtriangle(20, 1, 5, 3.5)
triangle_mle(xtest)
}
\references{
Samuel Kotz and Johan Rene van Dorp. Beyond Beta \doi{10.1142/5720}
}
