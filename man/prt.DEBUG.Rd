\name{prt.DEBUG}
\alias{prt.DEBUG}
\title{Utility Printing in DEBUG mode}
\description{
  This function prints out its arguments as \code{\link{cat}()} does,
  additionally printing the name of function in which it's been called ---
  only when a global variable \code{DEBUG} exists and is
  \code{\link{TRUE}}.\cr

  The global \code{DEBUG} has been a cheap precursor to the now standard
  \R's \code{\link{options}(verbose= .)} setting.
}
\usage{
prt.DEBUG(\dots, LEVEL = 1)
}
\arguments{
  \item{\dots}{arguments to be passed to \code{\link{cat}(\dots)} for
    printing.}
  \item{LEVEL}{integer (or logical) indicating a debugging level for printing.}
}
\author{Martin Maechler, originally for S-PLUS.}
\note{This is mainly kept for historical reasons (and old code
  fragments), but sometimes I still consider renaming it and have it
  work using \code{getOption("verbose")} alone.
}
\keyword{debugging}

