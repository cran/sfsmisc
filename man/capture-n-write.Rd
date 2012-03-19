\name{capture.and.write}
\alias{capture.and.write}
\title{Capture output and Write / Print First and Last Parts}
\usage{
capture.and.write(EXPR, first, last = 2, middle = NA,
                  i.middle, dotdots = " ....... ", n.dots = 2)
}
\arguments{
  \item{EXPR}{the (literal) expression the output of which is to be
    captured.}
  \item{first}{integer: how many lines should be printed at beginning.}
  \item{last}{integer: how many lines should be printed at the end.}
  \item{middle}{numeric (or NA logical):}
  \item{i.middle}{index start of middle part}
  \item{dotdots}{string to be used for elided lines}
  \item{n.dots}{number of \code{dotdots} ....}%% FIXME
}
\value{
  return value of \code{\link{writeLines}}
}
\description{
  Capture output and print first and last parts, eliding
  middle parts. P articularly useful for teaching purposes,
  and e.g., in Sweave
}
\author{
  Martin Maechler
}
