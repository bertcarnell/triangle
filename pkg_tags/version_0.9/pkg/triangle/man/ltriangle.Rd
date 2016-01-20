\name{ltriangle}
\alias{ltriangle}
\alias{dltriangle}
\alias{pltriangle}
\alias{qltriangle}
\alias{rltriangle}
\title{The Logarithmic Triangle Distribution}
\description{
These functions provide information about the triangle distribution on the
logarithmic interval from \code{a} to \code{b} with a maximum at \code{c}.  \code{dltriangle}
gives the density, \code{pltriangle} gives the distribution function,
\code{qltriangle} gives the quantile function, and \code{rltriangle} generates
\code{n} random deviates.
}
\usage{
dltriangle(x, a=1, b=100, c=10^((log10(a)+log10(b))/2), logbase=10)
pltriangle(q, a=1, b=100, c=10^((log10(a)+log10(b))/2), logbase=10)
qltriangle(p, a=1, b=100, c=10^((log10(a)+log10(b))/2), logbase=10)
rltriangle(n=1, a=1, b=100, c=10^((log10(a)+log10(b))/2), logbase=10)
}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{a}{lower limit of the distribution.}
  \item{b}{upper limit of the distribution.}
  \item{c}{mode of the distribution.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{logbase}{the base of the logarithm to use.}
}
\details{
  All probabilities are lower tailed probabilties.

  \code{a}, \code{b}, and \code{c} may be appropriate length vectors except in
  the case of \code{rtriangle}.
}
\value{
  \code{dltriangle} gives the density, \code{pltriangle} gives the distribution
  function, \code{qltriangle} gives the quantile function, and \code{rltraingle}
  generates random deviates.

  Invalid arguments will result in return value \code{NaN} or \code{NA}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\author{Rob Carnell}
\seealso{
  \code{\link{.Random.seed}} about random number generation,
  \code{\link{runif}}, etc for other distributions.
}
\examples{
## view the distribution
tri <- rltriangle(100000, 1, 100, 10)
hist(log10(tri), breaks=100, main="Triangle Distribution", xlab="x")

dltriangle(10, 1, 100, 10) # 2/(log10(b)-log10(a)) = 1

qltriangle(pltriangle(10)) # 10
}
\keyword{distribution}
