\name{plotCI}
\alias{plotCI}
\title{Plot Confidence Intervals / Error Bars}
\description{
  THIS IS NOW (2004-08-12) DEPRECATED !!!

  DO USE  \code{\link[gregmisc]{plotCI}} from package \pkg{gregmisc} INSTEAD!


  Given a set of x and y values and upper and lower bounds,
  this function plots the points with error bars.  Can be used for
  confidence interval visualization as well.
}
\usage{
plotCI(x, y = NULL, uiw, liw = uiw, aui = NULL, ali = aui,
       err = "y", xlim = NULL, ylim = NULL, type = "p", log = "",
       sfrac = 0.01, gap = 0, add = FALSE,
       col = par("col"), lwd = par("lwd"), slty = par("lty"),
       scol = col, pt.bg = NA,
       xlab = NULL, ylab = NULL, main = "", axes = TRUE, \dots)
}
\arguments{
  \item{x}{the x coordinates of points in the plot}
  \item{y}{the y coordinates of points in the plot}
  \item{uiw}{the width of the upper portion of the confidence region,
    or (if \code{liw} is missing) the width of both halves of
    the confidence region}
  \item{liw}{the width of the lower portion of the confidence region (if
    missing, the function assumes symmetric confidence bounds)}
  \item{aui}{the absolute upper limit of the confidence region}
  \item{ali}{the absolute lower limit of the confidence region}
  \item{err}{character giving the direction of error bars: \code{"x"}
    for horizontal, \code{"y"} for vertical (\code{"xy"} would be nice
    but is not implemented yet).}
  \item{xlim}{x limits of the plot}
  \item{ylim}{y limits of the plot}
  \item{type}{point/line type; passed to \code{\link{points}}}
  \item{log}{character indicating if log scales should be use in x or y;
    passed to \code{\link{plot.default}()}.}
  \item{gap}{size of gap in error bars around points (default 0)}
  \item{sfrac}{scaling factor for the size of the \code{serifs} (end bars) on
    the confidence bars, in x-axis units}
  \item{add}{logical; if FALSE (default), create a new plot; if TRUE,
    add error bars to an existing plot.}
  \item{col}{color of points}
  \item{lwd}{line width of error bars}
  \item{slty}{line type of error bars}
  \item{scol}{color of error bars}
  \item{pt.bg}{background color of points (use
    \code{pch=21, pt.bg=par("bg")} to get open points superimposed on
    error bars).}
  \item{xlab, ylab}{horizontal and vertical axis labels.}
  \item{main}{main title, see \code{\link{plot.default}} or
    \code{\link{title}}.}
  \item{axes}{logical indicating if axes should drawn, see
    \code{\link{plot.default}}.}
  \item{\dots}{any other parameters to be passed through to
    \code{\link{plot}}.}
}
\value{
None.
}
\author{Ben Bolker \email{bolker@zoo.ufl.edu}
  (documentation and tweaking of a function provided by Bill Venables)
}
\note{
  The package \pkg{gregmisc} has been containing another extension of
  Ben's original posting with the same name
  \code{\link[gregmisc]{plotCI}}.  We should really merging the two
  versions into Greg's one, i.e., eventually deprecate this version (of
  package \pkg{sfsmisc}).
}
\section{TO DO}{Fixes for logarithmic axes; simultaneous horizontal and
  vertical error bars.  The main problem is figuring how parameters
  should be specified.
}
\examples{
  y <- runif(10)
  err <- runif(10)
  plotCI(1:10,y,err)
  plotCI(1:10,y,err,2*err,lwd=2,col="red")
  err.x <- runif(10)
  err.y <- runif(10)
  plotCI(1:10,y,err.y,pt.bg=par("bg"),pch=21)
  plotCI(1:10,y,err.x,pt.bg=par("bg"),pch=21,err="x",add=TRUE)
}
\keyword{hplot}
%% 318 Carr Hall                                bolker@zoo.ufl.edu
%% Zoology Department, University of Florida    http://www.zoo.ufl.edu/bolker
%% Box 118525                                   (ph)  352-392-5697
%% Gainesville, FL 32611-8525                   (fax) 352-392-3704
