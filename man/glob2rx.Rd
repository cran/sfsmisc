\name{glob2rx}
\alias{glob2rx}
\title{Change Wildcard or Globbing Pattern into Regular Expression}
\description{
  Change \emph{wildcard} aka \emph{globbing} (or \dQuote{ls} like)
  pattern into the corresponding regular expression (\code{\link{regexp}}).
}
\usage{
glob2rx(pattern)
}
\arguments{
  \item{pattern}{character vector}
}
\value{
  a character vector of the same length as the input \code{pattern}
  where the \dQuote{wild card} is translated to the corresponding
  regular expression.
}
\author{Martin Maechler, Unix/sed based version, 1991; current: 2004}
\seealso{\code{\link{regexp}} about regular expression,
  \code{\link{sub}}, etc about substitutions using regexps.}
\examples{
stopifnot(glob2rx("abc.*") == "^abc\\\\.",% '\\' doubled in Rd file!
          glob2rx("a?b.*") == "^a.b\\\\.",
          glob2rx("*.doc") == "^.*\\\\.doc$",
          glob2rx("*.t*")  == "^.*\\\\.t",
          glob2rx("*.t??") == "^.*\\\\.t..$")
}
\keyword{file}
\keyword{character}
\keyword{utilities}

