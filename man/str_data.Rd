\name{str_data}
\alias{str_data}
\title{Overview on All Datasets in an R Package}
\description{
  Provide an overview over all datasets available by
  \code{\link{data}()} in a (list of) given \R packages.
}
\usage{
str_data(pkgs, \dots)
}
\arguments{
  \item{pkgs}{character vector of names of \R packages.}
  \item{\dots}{potentical further arguments to be passed to
    \code{\link{str}}; \code{str(utils:::str.default)} gives useful list.}
}
\value{
  as with \code{\link{str}()}, there's no return value.
  Everything is printed (via \code{\link{cat}}) to the console.
}
\author{Martin Maechler}
\seealso{\code{\link[utils]{str}}, \code{\link[utils]{data}}.
}
\examples{
str_data("cluster")

str_data("datasets", max=0, give.attr = FALSE)

### Data sets in all attached packages but "datasets" (and stubs):
s <- search()
(Apkgs <- sub("^package:", '', s[grep("^package:", s)]))
str_data(Apkgs[!Apkgs \%in\% c("datasets", "stats", "base")])
}
\keyword{datasets}
\keyword{utilities}
\keyword{documentation}

