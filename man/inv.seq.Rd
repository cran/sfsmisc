\name{inv.seq}
\alias{inv.seq}
\title{Inverse seq() -- Short Expression for Index Vector}
\description{
  Compute a short expression for a given integer vector, typically
  an index, that can be expressed shortly, using \code{\link{:}} etc.
}
\usage{
inv.seq(i)
}
\arguments{
  \item{i}{vector of (usually increasing) integers.}
}
%%\details{
%%
%%}
\value{
  an \code{\link{expression}} which can be \code{\link{eval}()}ed to
  return the original \code{i}.
}
\author{Martin Maechler, Oct. 1995}
\seealso{\code{\link{rle}} for another kind of integer vector coding.}
\examples{
(rr <- inv.seq(i1 <- c(3:12, 20:24, 27, 30:33)))
eval(rr)
stopifnot(eval(rr) == i1)

e2 <- expression(c(20:13, 3:12, -1:-4, 27, 30:31))
(r2 <- inv.seq(i2 <- eval(e2)))
stopifnot(identical(e2,r2))
}
\keyword{arith}
\keyword{utilities}
