\name{huberM}
\alias{huberM}
\title{Safe (generalized) Huber M-Estimator of Location}
\description{
  (Generalized) Huber M-estimator of location with MAD scale, being
  sensible also when the scale is zero where \code{\link[MASS]{huber}()}
  returns an error.
}
\usage{
huberM(x, k = 1.5, tol = 1e-06, mu = median(x), s = mad(x, center=mu),
       warn0scale = getOption("verbose"))
}
\arguments{
  \item{x}{numeric vector.}
  \item{k}{positive factor; the algorithm winsorizes at \code{k}
    standard deviations.}
  \item{tol}{convergence tolerance.}
  \item{mu}{initial location estimator.}
  \item{s}{scale estimator held constant through the iterations.}
  \item{warn0scale}{logical; if true, and \code{s} is 0 and
    \code{length(x) > 1}, this will be warned about.}
}
\value{
  list of location and scale parameters, and number of iterations used.
  \item{mu}{location estimate}
  \item{s}{the \code{s} argument,MAD scale estimate -- unless \code{s} is}
  \item{it}{the number of \dQuote{Huber iterations} used.}
}
\author{Martin Maechler building on the MASS code mentioned.}
\references{
  Huber, P. J. (1981)
  \emph{Robust Statistics.}
  Wiley.
}
\seealso{
  \code{\link[MASS]{hubers}} (and \code{huber}) in package \pkg{MASS};
  \code{\link{mad}}.
}
\examples{
huberM(c(1:9, 1000))
mad  (c(1:9, 1000))
mad  (rep(9, 100))
huberM(rep(9, 100))
}
\keyword{robust}
