\name{ellipsePoints}
\alias{ellipsePoints}
\title{Compute Radially Equispaced Points on Ellipse}
\description{
  Compute points on (the boundary of) an ellipse which is given by
  elementary geometric parameters.
}
\usage{
ellipsePoints(a, b, alpha = 0, loc = c(0, 0), n = 201)
}
\arguments{
  \item{a,b}{length of half axes in (x,y) direction.}
  \item{alpha}{angle (in degrees) giving the orientation of the ellipse,
    i.e., the original (x,y)-axis ellipse is rotated by \code{angle}.}
  \item{loc}{center (LOCation) of the ellipse.}
  \item{n}{number of points to generate.}
}
\value{
  A numeric matrix of dimension \code{n x 2}, each row containing the
  (x,y) coordinates of a point.
}
\author{Martin Maechler, March 2002.}
\seealso{the \file{ellipse} package and \code{\link[cluster]{ellipsoidhull}}
 and \code{\link[cluster]{ellipsoidPoints}}
  in the \file{cluster} package.
}
\examples{
## Simple Ellipse, centered at (0,0), x-/y- axis parallel:
ep <- ellipsePoints(2,5)
str(ep)
plot(ep, type="n",asp=1) ; polygon(ep, col = 2)

## rotate by 30 degrees :
plot(ellipsePoints(2,5, alpha = 30), asp=1)
abline(h=0,v=0,col="gray")
abline(a=0,b= tan( 30 *pi/180), col=2, lty = 2)
abline(a=0,b= tan(120 *pi/180), col=3, lty = 2)

if(dev.interactive()) {
  ## Movie : rotating ellipse  :
  nTurns <- 4 # #{full 360 deg turns}
  for(al in 1:(nTurns*360)) {
      ep <- ellipsePoints(3,6, alpha=al, loc = c(5,2))
      plot(ep,type="l",xlim=c(-1,11),ylim=c(-4,8),
	   asp=1, axes = FALSE, xlab="", ylab="")
  }

  ## Movie : rotating _filled_ ellipse {less nice to look at}
  for(al in 1:180) {
      ep <- ellipsePoints(3,6, alpha=al, loc = c(5,2))
      plot(ep,type="n",xlim=c(-1,11),ylim=c(-4,8),
	   asp=1, axes = FALSE, xlab="", ylab="")
      polygon(ep,col=2,border=3,lwd=2.5)
  }
}# only if interactive
}
\keyword{iplot}
\keyword{utilities}
