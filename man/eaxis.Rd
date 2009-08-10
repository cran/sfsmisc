\name{eaxis}
\alias{eaxis}
\title{Extended / Engineering Axis for Graphics}
\description{
  An extended \code{\link[graphics]{axis}()} function which labels more
  prettily, in particular for log-scale axes.

  It  makes use of \link{plotmath} \code{\link{expression}}s of
  the form \eqn{k \times 10^k}{k * 10^k} for labeling a
  log-scaled axis and when otherwise exponentional formatting would be
  used.
}
\usage{
eaxis(side, at = axTicks(side, log=log), labels = NULL, log = NULL,
      f.smalltcl = 3/5, at.small = NULL, small.mult = NULL,
      outer.at = TRUE, drop.1 = TRUE, las = 1, \dots)
}
\arguments{
  \item{side}{integer in 1:4, specifying side of \code{\link{axis}}.}
  \item{at}{numeric vector of (\dQuote{normalsized}) tick locations; by
    default \code{\link[graphics]{axTicks}(side)}, i.e., the same as
    \code{\link{axis}()} would use.}
  \item{labels}{\code{NULL} (default), \code{\link{logical}},
    \code{character} or \code{expression}, as in \code{\link{axis}()};
    in addition, if \code{NA}, \code{labels = TRUE} is passed to
    \code{\link{axis}()}, i.e. \code{\link{pretty10exp}} is \emph{not}
    used.  Use \code{FALSE} to suppress any labeling.}
  \item{log}{logical or \code{NULL} specifying if log-scale should be
    used; the default depends on the current plot's axis.}
  \item{f.smalltcl}{factor specifying the lengths of the small ticks in
    proportion to the normalsized, labeled ticks.}
  \item{at.small}{locations of \emph{small} ticks; default uses
    used as \dQuote{small ticks}.}
  \item{small.mult}{which multiples of \code{\link{axTicks}()} should be
    used as \dQuote{small ticks} by default.}
  \item{outer.at}{logical specifying that \code{at.small} should also be
    constructed outside the \code{at} range, but still inside the
    corresponding \code{\link{par}("usr")}.}
  \item{drop.1}{logical specifying if \code{"1 *"} should be dropped
    from labels, passed to \code{\link{pretty10exp}()}.}
  \item{las, \dots}{arguments passed to (the first) \code{\link{axis}}
    call.  Note that the default \code{las = 1} differs from
    \code{axis}'s default \code{las = 0}.}
}
\author{Martin Maechler}
\seealso{\code{\link[graphics]{axis}},
  \code{\link[graphics]{axTicks}},
  \code{\link{axTexpr}},
  \code{\link{pretty10exp}}.
}
\examples{
x <- lseq(1e-10, 0.1, length = 201)
plot(x, pt(x, df=3), type = "l", xaxt = "n", log = "x")
eaxis(1)

## If you like the ticks, but prefer traditional (non-"plotmath") labels:
plot(x,  gamma(x),   type = "l", log = "x")
eaxis(1, labels=NA)

x <- lseq(.001, 0.1, length = 1000)
plot(x, sin(1/x)*x, type = "l", xaxt = "n", log = "x")
eaxis(1)

## non- log-scale : draw small ticks, but no "10^k" if not needed:
x <- seq(-100, 100, length = 1000)
plot(x, sin(x)/x, type = "l", xaxt = "n")
eaxis(1)

x <- seq(-1, 1, length = 1000)
plot(x, sin(x)/x, type = "l", xaxt = "n")
eaxis(1)

x <- x/1000
plot(x, 1-sin(x)/x, type = "l", xaxt = "n", yaxt = "n")
eaxis(1)
eaxis(2)
## more labels than default:
op <- par(lab=c(10,5,7))
plot(x, sin(x)/x, type = "l", xaxt = "n")
eaxis(1) # maybe (depending on your canvas), there are too many,
## in that case, maybe use
plot(x, sin(x)/x, type = "l", xaxt = "n")
eaxis(1, axTicks(1)[c(TRUE,FALSE)]) # drop every 2nd label
eaxis(3, labels=FALSE)
par(op)
}
\keyword{aplot}
