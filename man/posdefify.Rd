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
  \item{method}{a string specifying the method to apply; can be abbreviated.}
  \item{symmetric}{logical, simply passed to \code{\link{eigen}}.}
  \item{eps.ev}{number specifying the tolerance to use, see Details
    below.}
}
\details{
  We form the eigen decomposition
  \deqn{m = V \Lambda V'}{m = V L V'} where \eqn{\Lambda}{L} is the
  diagonal matrix of eigenvalues, \eqn{\Lambda_{j,j} = \lambda_j}{L[j,j]
    = l[j]}, with \emph{decreasing} eigenvalues \eqn{\lambda_1 \ge
    \lambda_2 \ge \ldots \ge \lambda_n}{l[1] >= l[2] >= ... >= l[n]}.

  When the smallest eigenvalue \eqn{\lambda_n}{l[n]} are less than
  \code{Eps <- eps.ev * abs(lambda[1])}, i.e., negative or \dQuote{almost
    zero}, some or all eigenvalues are replaced by \emph{positive}
  (\code{>= Eps}) values,
  \eqn{\tilde\Lambda_{j,j} = \tilde\lambda_j}{L~[j,j] = l~[j]}.
  Then, \eqn{\tilde m = V \tilde\Lambda V'}{m~ = V L~ V'} is computed
  and rescaled in order to keep the original diagonal (where that is
  \code{>= Eps}).
}
\value{
  a matrix of the same dimensions and the \dQuote{same} diagonal
  (i.e. \code{\link{diag}}) as \code{m} but with the property to
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
