\name{p.res.2x}
\alias{p.res.2x}
\title{Stahel's Residual Plot against 2 X's}
\description{
  Plot Residuals (e.g., of a multiple linear regression)
}
\usage{
p.res.2x(x, y, z, restricted, size = 1, slwd = 1, scol = 2,
         xlab = NULL, ylab = NULL, ...)
}
\arguments{
  \item{x,y}{numeric vectors of the same length specifying 2 covariates.}
  \item{z}{numeric vector of same length as \code{x} and \code{y},
    typically residuals.}
  \item{restricted}{positive value which truncates the size.  The
    corresponding symbols are marked by stars.}
  \item{size}{the symbols are scaled so that \code{size} is the size of
    the largest symbol in cm.}
  \item{slwd, scol}{line width and color for the residual
    \code{\link{segments}}.}
  \item{xlab,ylab}{axis labels, see \code{\link{plot.default}}.}
  \item{\dots}{further arguments passed to \code{plot}.}
}
\details{
  ...........
  ..........
}
\references{Stahel, W. (1996) ........}
\author{Andreas Ruckstuhl in June 1991 and Martin Maechler, in 1992, '94.}
\seealso{\code{\link{p.res.2fact}}, \code{\link{plot.lm}},
   \code{\link{TA.plot}}.}

\examples{
xx <- rep(1:10,7)
yy <- rep(1:7, rep(10,7))
zz <- rnorm(70)
p.res.2x(xx,yy,zz, restr = 2, main = "i.i.d.  N(0,1) random residuals")

%%example(lm, echo = FALSE)
%%% ADD-- FIXME
}
\keyword{hplot}
\keyword{regression}
