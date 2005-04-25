\name{rnls}
\alias{rnls}
\title{Robust Nonlinear Regression}
\description{
  Compute robustified nonlinear regressions, i.e.,
  robustly estimate parameters in the nonlinear model.  The fitting is
  done by iterated reweighted least squares (IWLS) as in
  \code{\link[MASS]{rlm}}() of package \pkg{MASS}.  Additionally, see
  \code{\link{nls}}.
}
\usage{
rnls(formula, data, start, weights = NULL,
     na.action = na.fail, psi = MASS::psi.huber,
     test.vec = c("resid", "coef", "w"),
     maxit = 20, acc = 1e-06, algorithm = "default",
     control, trace = FALSE, \dots)
}
%% FIXME: use
%% ----- ../R/rnls.R  and ~/R/D/r-devel/R/src/library/stats/man/nls.Rd
\arguments{
  \item{formula}{a nonlinear formula including variables and parameters and
    which results in the residuals of the model, e.g. \code{~ y - f(x,
      alpha)}. (For some checks: if \eqn{f(.)} is linear, then we need
    parentheses, e.g., \code{ ~ y - (a + b * x)}.)
%% FIXME in code:
    Do not use as \code{w} as variable or parameter name!
  }
  \item{data}{a data frame in which to do the computations.}
  \item{start}{a named numeric vector of starting parameters estimates.}
  \item{weights}{an optional weighting vector (for intrinsic weights, not
    the weights \code{w} used in the iterative (robust) fit). I.e.,
    \code{sum(w * e^2)} is minimized with \code{e} = residuals,
    \eqn{e_i = y_{i} - f(xreg_{i}, alpha's)}{e[i] = y[i] - f(xreg[i], alpha's)},
    and \code{w} are the robust weights from \code{resid * weights}.}
  \item{na.action}{a function which indicates what should happen when the data
    contain \code{NA}s.  The default action is for the procedure to
    fail.  If NAs are present, use \code{na.exclude} to have residuals with
    \code{length == nrow(data) == length(w)}, where \code{w} are the
    weights used in the iterative robust loop.  This is better if the vars in
    \code{formula} are time series (and so the NA location is important).
    For this reason, \code{na.omit}, which leads to omission of cases
    with missing values on any required variable, is not suitable
    here since the residuals length is different from \code{nrow(data)
      == length(w)}.}
  \item{psi}{a function (possibly by name) of the form \code{g(x, \dots,
      deriv)} that for \code{deriv=0} returns \eqn{\psi(x)/x} and for
    \code{deriv=1} returns \eqn{\psi'(x)}.  Tuning constants will be
    passed in via \code{...}.
    Psi functions are supplied by package \pkg{MASS} for the Huber
    (the default), Hampel and Tukey bisquare proposals as
    \code{psi.huber}, \code{psi.hampel} and \code{psi.bisquare} (see
    \code{\link[MASS]{rlm}}).}
  \item{test.vec}{character string specifying the convergence criterion.
    The relative change is tested for residuals with a value of
    \code{"resid"} (the default), for coefficients with \code{"coef"},
    and for weights with \code{"w"}.}
  \item{maxit}{maximum number of iterations in the robust loop.}
  \item{acc}{convergence tolerance for the robust fit.}
  \item{algorithm}{character string specifying the algorithm to use for
    \code{nls}.  The default algorithm is a Gauss-Newton algorithm.  The
    other alternative is "plinear", the Golub-Pereyra algorithm for
    partially linear least-squares models.}
  \item{control}{an optional list of control settings for \code{\link{nls}}.
    See \code{\link{nls.control}} for the names of the settable control
    values and their effect.}
  \item{trace}{logical value indicating if a \dQuote{trace} of
    the \code{nls} iteration progress should be printed.  Default is
    \code{FALSE}. \cr
    If \code{TRUE}, in each robust iteration, the residual
    sum-of-squares and the parameter values are printed at the
    conclusion of each \code{nls} iteration.
    When the \code{"plinear"} algorithm is used, the conditional
    estimates of the linear parameters are printed after the nonlinear
    parameters.}
  \item{\dots}{potentially arguments to be passed to the \code{psi}
    function (see above).}
}
% \details{
% }
\value{
  An object of S3 class \code{"nls.robust"}, inheriting from class
  \code{"nls"}, (see \code{\link{nls}}).
  It is a list with components
%% FIXME
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
%\references{ ~put references to the literature/web site here ~ }
\author{
  Andreas Ruckstuhl (inspired by \code{\link[MASS]{rlm}}() and
  \code{\link{nls}}()), in July 1994 for S-plus.\cr
  Christian Sangiorgio did the update to \R and corrected some errors,
  from June 2002 to January 2005.\cr
  Help page, testing, more cleanup, methods: Martin Maechler.
}
%%\note{ ~~further notes~~ }

\seealso{ \code{\link{nls}}, \code{\link[MASS]{rlm}}.
}
\examples{
DNase1 <- DNase[ DNase$Run == 1, ]

## note that selfstarting models doesn't work yes % <<< FIXME !!!

##--- without conditional linearity ---

## classical
fm3DNase1 <- nls( density ~ Asym/(1 + exp(( xmid - log(conc) )/scal ) ),
                  data = DNase1,
                  start = list( Asym = 3, xmid = 0, scal = 1 ),
                  trace = TRUE )
summary( fm3DNase1 )

## robust
frm3DNase1 <- rnls( ~ density - Asym/(1 + exp(( xmid - log(conc) )/scal ) ),
                  data = DNase1, trace = TRUE,
                  start = list( Asym = 3, xmid = 0, scal = 1 ))
## FAILS (summary.nls): summary( frm3DNase1 )%% <<< FIXME

##--- using conditional linearity ---

## classical
fm2DNase1 <- nls( density ~ 1/(1 + exp(( xmid - log(conc) )/scal ) ),
                  data = DNase1,
                  start = c( xmid = 0, scal = 1 ),
                  alg = "plinear", trace = TRUE )
summary( fm2DNase1 )

## robust
if(FALSE) { # currently fails %% FIXME error in nls's nlsModel.plinear()
frm2DNase1 <- rnls( ~ density - 1/(1 + exp(( xmid - log(conc) )/scal ) ),
                  data = DNase1, start = c( xmid = 0, scal = 1 ),
                  alg = "plinear", trace = TRUE )
summary( frm2DNase1 )
} # not yet

}
\keyword{robust}
\keyword{regression}
