\name{Sys.cpuinfo}
\alias{Sys.procinfo}
\alias{Sys.cpuinfo}
\alias{Sys.meminfo}
\alias{Sys.MIPS}
\title{Provide Information about the Linux Hardware (CPU, Memory, etc)}
\description{
  Return information about the Linux hardware, notably
  the CPU (the central processor unit) and memory of the
  computer \R is running on.
  This is currently \bold{only available for Linux}.

  These functions exist on other unix-alike platforms, but produce an
  error when called.
}
\usage{
Sys.procinfo(procfile)
Sys.cpuinfo()
Sys.meminfo()
Sys.MIPS()
}
\arguments{
  \item{procfile}{name of file the lines of which give the CPU info ``as
    on Linux''}
}
\value{
  The \code{Sys.*info()} functions return a \code{"simple.list"},
  here basically a named character vector,
  whereas \code{Sys.MIPS} returns a number giving an approximation of
  the \bold{M}illion \bold{I}instructions \bold{P}er \bold{S}econd that
  the CPU processes.  This is a performance measure of the basic
  \emph{non-numeric} processing capabilities and for Linux systems often
  about twice the basic clock rate in ``MHz'' as available by
  \code{Sys.cpuinfo()["cpu MHz"]}.
}
\author{Martin Maechler}
\note{These currently do rely on the Linux \file{/proc/} file system, and may not
  easily be portable to non-Linux environments.

  On multi-processor machines, \code{Sys.cpuinfo()} contains each field
  for each processor (i.e., \code{names(Sys.cpuinfo())} has
  \code{\link{duplicated}} entries).

  Conceivably, the bogoMIPS source code is open and available and could
  be built into \R.
}
\seealso{\code{\link{Sys.ps}}, etc.}

\examples{
if(substr(R.version[["os"]], 1,5) == "linux") { ##-- only on Linux

  Sys.cpuinfo() # which is often ugly
  local({I <- Sys.cpuinfo(); I[ "flags" != names(I) ] })
  Sys.MIPS()
  Sys.MIPS() / as.numeric(Sys.cpuinfo()["cpu MHz"]) ## < often about 2
}
}
\keyword{utilities}%-- one or more ...
