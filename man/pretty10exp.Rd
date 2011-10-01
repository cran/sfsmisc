\name{pretty10exp}
\alias{pretty10exp}
\title{Nice  10 ** k  Label Expressions}
\description{
  Produce nice \eqn{a \times 10^k}{a * 10^k} expressions to be used
  instead of the scientific notation  \code{"a E<k>"}.
}
\usage{
pretty10exp(x, drop.1 = FALSE, digits.fuzz = 7)
}
\arguments{
  \item{x}{numeric vector (e.g. axis tick locations)}
  \item{drop.1}{logical indicating if \eqn{1 \times}{1 *} should be
    dropped from the resulting expressions.}
  \item{digits.fuzz}{number of digits to be considered for integer
    equality; do not change lightly!}
}
\value{
  an expression of the same length as \code{x}, with elements of the
  form \code{a \%*\% 10 ^ k}.
}
\author{Martin Maechler}
\seealso{\code{\link{axTexpr}} which builds on \code{pretty10exp()};
  further \code{\link{axis}}, \code{\link{axTicks}}.
}
\examples{
pretty10exp(-1:3 * 1000)
pretty10exp(-1:3 * 1000, drop.1 = TRUE)
pretty10exp(c(1,2,5,10,20,50,100,200) * 1e3)
\dontshow{
stopifnot(identical(pretty10exp(numeric(0)), expression()))
}
ax <- 10^(-6:0) - 2e-16
pretty10exp(ax, drop.1=TRUE)
## in sfsmisc version <= 1.0-16, no 'digits',
## i.e., implicitly had  digits := #{double precision digits} ==
(dig. <- .Machine$double.digits * log10(2)) # 15.95
pretty10exp(ax, drop.1=TRUE, digits= dig.)  # ``ugly''
}
\keyword{dplot}
