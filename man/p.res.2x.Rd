\name{p.res.2x}
\alias{p.res.2x}
\title{Stahel's Residual Plot against 2 X's}
\description{
  Plot Residuals (e.g., of a multiple linear regression)
}
\usage{
p.res.2x(x, y, z, restricted, size = 1, slwd = 1, scol = 2:3,
         xlab = NULL, ylab = NULL, main = NULL,
         xlim = range(x), ylim = range(y), \dots)
}
\arguments{
  \item{x,y}{numeric vectors of the same length specifying 2 covariates.}
  \item{z}{numeric vector of same length as \code{x} and \code{y},
    typically residuals.}
  \item{restricted}{positive value which truncates the size.  The
    corresponding symbols are marked by stars.}
  \item{size}{the symbols are scaled so that \code{size} is the size of
    the largest symbol in cm.}
  \item{slwd, scol}{line width and color(s) for the residual
    \code{\link{segments}}.  If \code{scol} has length 2 as per default,
    the two colors are used for positive and negative \code{z} values,
    respectively.}
  \item{xlab, ylab, main}{axis labels, and title see \code{\link{title}},
    each with a sensible default.  To suppress, use, e.g., \code{main = ""}.}
  \item{xlim, ylim}{the basic x- and y- axis extents, see
    \code{\link{plot.default}}.  Note that these will be slightly
    extended such that segments are not cut off.}
  \item{\dots}{further arguments passed to \code{plot}.}
}
\details{
  ...........
  ..........
}
\references{Stahel, W. (1996) ........}
\author{Andreas Ruckstuhl in June 1991 and
  Martin Maechler, in 1992, '94, 2003-4.}
\seealso{\code{\link{p.res.2fact}}, \code{\link{plot.lm}},
   \code{\link{TA.plot}}.}

\examples{
xx <- rep(1:10,7)
yy <- rep(1:7, rep(10,7))
zz <- rnorm(70)
p.res.2x(xx,yy,zz, restr = 2, main = "i.i.d.  N(0,1) random residuals")
\dontshow{
 p.res.2x(xx,yy,zz, restr = 2, main = "p.res.2x(*, xlim, ylim)",
          xlim = c(2,8), ylim = c(1,4))
}
example(lm.influence, echo = FALSE)
mult.fig(2, marP=c(0,-1,-2,0), main="p.res.2x(*,*, residuals(lm.SR))")
with(LifeCycleSavings,
     { p.res.2x(pop15, ddpi, residuals(lm.SR), scol=c("red", "blue"))
       p.res.2x(pop75, dpi,  residuals(lm.SR), scol=2:1)
     })
}
\keyword{hplot}
\keyword{regression}
