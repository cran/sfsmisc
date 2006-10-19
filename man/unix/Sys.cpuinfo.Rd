\name{Sys.cpuinfo}
\alias{Sys.cpuinfo}
\alias{Sys.MIPS}
\title{Provide Information about the CPU}
\description{
  Return information about the CPU, the central processor unit of the
  computer \R is running on.
  This is currently \bold{only available for Linux}.
}
\usage{
Sys.cpuinfo(procfile = "/proc/cpuinfo")
Sys.MIPS()
}
\arguments{
  \item{procfile}{name of file the lines of which give the CPU info ``as
    on Linux''}
}
\value{
  \code{Sys.cpuinfo()} returns a named character vector, \code{Sys.MIPS}
  a number giving an approximation of the \bold{M}illion
  \bold{I}instructions \bold{P}er \bold{S}econd that the CPU
  processes. This is a performance measure of the basic
  \emph{non-numeric} processing capabilities and for Linux systems often
  about twice the basic clock rate in ``MHz'' as available by
  \code{Sys.cpuinfo()["cpu MHz"]}.
}
\author{Martin Maechler}
\note{These currently do rely on \code{cat /proc/cpuinfo}, and may not
  easily be portable to non-Linux environments.

  Conceivably, the bogoMIPS source code is open and available and could
  be built into \R.
}
\seealso{\code{\link{Sys.ps}}, etc.}

\examples{
if(substr(R.version[["os"]], 1,5) == "linux") { ##-- only on Linux
  cbind(Sys.cpuinfo())
  Sys.MIPS()
  Sys.MIPS() / as.numeric(Sys.cpuinfo()["cpu MHz"]) ## < often about 2
}
}
\keyword{utilities}%-- one or more ...
