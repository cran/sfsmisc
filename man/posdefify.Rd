\name{posdefify}
\alias{posdefify}
\title{Find a Close Positive Definite Matrix}
\description{
  From a matrix \code{m}, construct a \emph{"close"} positive definite
  one.
}
\usage{
posdefify(m, method = c("someEVadd", "allEVadd"), symmetric, eps.ev = 1e-07)
}
\arguments{
  \item{m}{a numeric (square) matrix.}
  \item{method}{a string specifying the method to apply.}
  \item{symmetric}{logical, simply passed to \code{\link{eigen}}.}
  \item{eps.ev}{number specifying the tolerance to use, see Details
    below.}
}
\details{
  We form the eigen decomposition
  \deqn{m = V \Lambda V'}
}
\value{
  a matrix of the same dimensions as \code{m} but with the property to
  be positive definite.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Martin Maechler}
\seealso{\code{\link{eigen}} on which the current methods rely.}
\examples{
 set.seed(12)
 m <- matrix(round(rnorm(25),2), 5, 5); m <- 1+ m + t(m); diag(m) <- diag(m) + 4
 m
 posdefify(m)
 1000 * zapsmall(m - posdefify(m))
}
\keyword{algebra}
\keyword{array}

