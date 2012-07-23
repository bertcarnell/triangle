\name{triangle}
\alias{triangle}
\alias{dtriangle}
\alias{ptriangle}
\alias{qtriangle}
\alias{rtriangle}
\title{The Triangle Distribution}
\description{
These functions provide information about the triangle distribution on the
interval from \code{a} to \code{b} with a maximum at \code{c}.  \code{dtriangle}
gives the density, \code{ptriangle} gives the distribution function,
\code{qtriangle} gives the quantile function, and \code{rtriangle} generates
\code{n} random deviates.
}
\usage{
dtriangle(q, a=0, b=1, c=0.5)
ptriangle(q, a=0, b=1, c=0.5)
qtriangle(p, a=0, b=1, c=0.5)
rtriangle(n, a=0, b=1, c=0.5)
}
\arguments{
  \item{q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{a}{lower limit of the distribution.}
  \item{b}{upper limit of the distribution.}
  \item{c}{mode of the distribution.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
}
\details{
  All probabilities are lower tailed probabilties.

  \code{a}, \code{b}, and \code{c} may be appropriate length vectors except in
  the case of \code{rtriangle}.
  
  \code{rtriangle} is derived from a draw from \code{runif}.

  The triangle distribution has density:
  \deqn{f(x) = \frac{2(x-a)}{(b-a)(c-a)}}{f(x) = 2(x-a) / [(b-a)(c-a)]}
  for \eqn{a \le x < c}{a <= x < c}.
  \deqn{f(x) = \frac{2(b-x)}{(b-a)(b-c)}}{f(x) = 2(b-x) / [(b-a)(b-c)]}
  for \eqn{c \le x \le b}{c <= x <= b}.
  \eqn{f(x) = 0} elsewhere.

  The mean and variance are:
  \deqn{E(x) = \frac{(a + b + c)}{3}}{E(x) = (a + b + c) / 3}
  \deqn{V(x) = \frac{1}{18}(a^2 + b^2 + c^2 - ab - ac - bc)}{V(x) = (a^2 + b^2 + c^2 - ab - ac - bc) / 18}
}
\value{
  \code{dtriangle} gives the density, \code{ptriangle} gives the distribution
  function, \code{qtriangle} gives the quantile function, and \code{rtraingle}
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
tri <- rtriangle(100000, 1, 5, 3)
hist(tri, breaks=100, main="Triangle Distribution", xlab="x")

mean(tri) # 1/3*(1 + 5 + 3) = 3
var(tri)  # 1/18*(1^2 + 3^2 + 5^2 - 1*5 - 1*3 - 5*3) = 0.666667

dtriangle(0.5, 0, 1, 0.5) # 2/(b-a) = 2

qtriangle(ptriangle(0.7)) # 0.7
}
\keyword{distribution}
