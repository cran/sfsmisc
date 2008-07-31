\name{AsciiToInt}
\encoding{latin1}
\alias{AsciiToInt}
\alias{ichar}
\alias{chars8bit}
\alias{strcodes}
\title{Character to and from Integer Codes Conversion}
\description{
  \code{AsciiToInt} returns \code{\link{integer}} codes in \code{0:255}
  for each (one byte) character in \code{strings}.  \code{ichar} is an
  alias for it, for old S compatibility.

  \code{strcodes} implements in \R the basic engine for translating
  characters to corresponding integer codes.

  \code{chars8bit()} is the \emph{inverse} function of
  \code{AsciiToint}, producing (one byte) characters from integer codes.
}
\usage{
AsciiToInt(strings)
     ichar(strings)
chars8bit(i = 1:255)
strcodes(x, table = chars8bit(1:255))
}
\arguments{
  \item{strings, x}{\code{\link{character}} vector.}
  \item{i}{numeric (integer) vector of values in \code{1:255}.}
  \item{table}{a vector of (unique) character strings, typically of one
  character each.}
}
\details{
  Only codes in \code{1:127} make up the ASCII encoding which should be
  identical for all \R versions, whereas the \emph{\sQuote{upper}} half
  is often determined from the ISO-8859-1 (aka \dQuote{ISO-Latin 1)}
  encoding, but may well differ, depending on the locale setting, see
  also \code{\link{Sys.setlocale}}.

  Note that \code{0} is no longer allowed since, \R does not allow
  \code{\\0} aka \code{nul} characters in a string anymore.
}
\value{
  \code{AsciiToInt} (and hence \code{ichar}) and \code{chars8bit} return a
  vector of the same length as their argument.

  \code{strcodes(x, tab)} returns a \code{\link{list}} of the same
  \code{\link{length}} and \code{\link{names}} as \code{x} with list
  components of integer vectors with codes in \code{1:255}.
}
\author{Martin Maechler, partly in 1991 for S-plus}
\examples{
chars8bit(65:70)#-> "A" "B" .. "F"
stopifnot(identical(LETTERS,   chars8bit(65:90)),
          identical(AsciiToInt(LETTERS), 65:90))
\dontrun{% The "ü" cannot even be parsed when in UTF-8 locale (R 2.1.0)
## may only work in ISO-latin1 locale (not in UTF-8):
strcodes(c(a= "ABC", ch="1234", place = "Zürich"))
## gives
$a
[1] 65 66 67

$ch
[1] 49 50 51 52

$place
[1]  90 252 114 105  99 104
}
}
\keyword{manip}
%\keyword{utilities}
