## From: Christian Sangiorgio <sangior@stat.math.ethz.ch>
## To: maechler@stat.math.ethz.ch
## Subject: Funktion rnls: 2 Entwurf
## Date: Mon, 17 Jan 2005 09:29:06 +0100

## Ciao Martin

## beim Lesen der rnls Funktion habe ich noch einen Fehler
## gefunden und zwar im Fall wo die Gewichte ('weights') geben
## sind. Früher hat man die  variable 'w' nach der robustes Schleife
## definiert und mit ihr die resid und fitted definiert... ohne
## berücksichtigung der externe  Gewichte.

## Hier die neue Funktion

rnls <- function(form, data, start, weights = NULL, na.action = na.fail,
                 psi = MASS::psi.huber,
                 test.vec = "resid", maxit = 20, acc = 1e-06,
                 algorithm = "default", control, trace = FALSE, ...)
{
    ## Purpose:
    ##  Robust parameters estimation in the nonlinear model. The fitting is
    ##  done by iterated reweighted least squares (IWLS) as in rlm() of the
    ##  package MASS. In addition, see also 'nls'.
    ##
    ## Arguments:
    ##   form     : a nonlinear formula including variables and parameters and
    ##              which results in the residuals of the model,
    ##              e.g. ~ y - f(x, alpha).
    ##              (For some checks:
    ##              if f(.) is linear, then we need parenthesis,
    ##              e.g. ~ y - (a + b * x)
    ##              Do not use as a variable name or as parameter name "w"!
    ##   data     : a data frame in which to do the computations.
    ##   start    : a named numeric vector of starting parameters estimate.
    ##   weights  : an optional weighting vector (for intrinsic weights, not
    ##              the weights 'w' used in the iterative (robust) fit). I.e.
    ##              'sum(w * e^2)' is minimized with
    ##               e = residuals: y_{t} - f(xreg_{t}, alpha's)
    ##               w = robust weights from resid * weights
    ##   na.action: a function which indicates what should happen when the data
    ##              contain 'NA's. The default action is for the procedure to
    ##              fail.
    ##              If NAs are present, use 'na.exclude' to have residuals with
    ##              the length == nrow(data) == lenght(w), 'w' = weights used
    ##              in the iterative robust loop. This is better if the vars in
    ##              'form' are time series (and so the NA location is important).
    ##              For this reason, 'na.omit', which leads to omission of cases
    ##              with missing values on any required variable, is not suitable
    ##              here since the residuals length is != from nrow(data) =
    ##              = length(w).
    ##   psi      : the psi function is specified by this argument.
    ##              It must give (possibly by name) a function 'g(x, ..., deriv)'
    ##              that for 'deriv=0' returns psi(x)/x and for 'deriv=1' returns
    ##              psi'(x). Tuning constants will be passed in via '...'.
    ##              Psi functions are supplied for the Huber (the default), Hampel
    ##              and Tukey bisquare proposals as 'psi.huber', 'psi.hampel' and
    ##              'psi.bisquare'. (see rlm())
    ##  test.vec  : character string specifying the convergence criterion. The
    ##              relative change is tested for residuals with a value of
    ##              "resid", for coefficients with "coef", and for weights with
    ##              "w".
    ##  maxit     : maximum number of iterations in the robust loop.
    ##  acc       : convergence tolerance for the robust fit.
    ##  algorithm : character string specifying the algorithm to use for 'nls'.
    ##              The default algorithm is a Gauss-Newton algorithm. The other
    ##              alternative is "plinear", the Golub-Pereyra algorithm for
    ##              partially linear least-squares models.
    ##  control   : an optional list of control settings for 'nls'.
    ##              See 'nls.control' for the names of the settable control
    ##              values and their effect.
    ##  trace     : logical value indicating if a trace of the 'nls' iteration
    ##              progress should be printed.  Default is 'FALSE'. If 'TRUE' in
    ##              each robust iteration, the residual sum-of-squares and the
    ##              parameter values are printed at the conclusion of each 'nls'
    ##              iteration.  When the '"plinear"' algorithm is used, the
    ##              conditional estimates of the linear parameters are printed
    ##              after the nonlinear parameters.
    ##
    ## Author: (inspired by rlm(), nls())
    ##         Andreas Ruckstuhl, Date: Jul 94
    ##         update (S+ => R, correction some errors): Ch. Sangiorgio, Jun 2002
    ## -------------------------------------------------------------------------

    ## to DO: summary, print method for this class ...

    irls.delta <- function(old, new)
        sqrt(sum((old - new)^2, na.rm = TRUE) / max(1e-20, sum(old^2,na.rm=TRUE)))

    ##- some checks
    form <- as.formula(form)
    if (length(form) != 2)
        stop("'form' should be a formula of the type ~ y - f(x, alpha)")

    nam.form <- all.vars(form)

    data <- as.data.frame(data)

    if (!(any(test.vec == c("resid", "coef", "w"))))
        stop("Invalid testvec")

    if (!is.vector(start) | !is.numeric(start) | any(is.na(start)) |
        is.null(names(start)) | any(is.na(match(names(start), nam.form))))
        stop("'start' should be a named numeric vector of the pars in 'form'")

    if (!is.na(match("w", nam.form)) | !is.na(match("w", names(start))) |
        !is.na(match("w", names(data))))
        stop("Do not use 'w' as a variable name or as a parameter name")

    if (!is.null(weights)){
        if (length(weights) != nrow(data))
            stop("'weights' should lenght == nr of obs")
        if (any(weights < 0) | any(is.na(weights)))
            stop("'weights' should be nonnegative and not contain NAs")
    }

    ##-   if (any(is.na(data)) & options("na.action")$na.action == "na.omit")
    ##-    stop("if NAs are present, use 'na.exclude' to preserve the residuals length")

    ##- initialize testvec  and update form with robust weights
    coef <- start
    resid <- eval(form[[2]], c(as.list(data), start)) ##- residual: y - f()
    w <- rep(1, nrow(data))
    if (!is.null(weights))
        w <- w * weights

    oform <- form
    form  <- as.formula(substitute(~ (RHS) * w, list(RHS = form[[2]])))
    ##- form <- update.formula(form, ~ I((.) * w))  ##- old

    ##- robust loop
    converged <- FALSE
    status <- "converged"
    method.exit <- FALSE

    for(iiter in 1:maxit){
        if(trace)
            cat(paste("robuste Iteration",iiter,"\n"))
        previous <- get(test.vec)
        Scale <- median(abs(resid), na.rm=TRUE)/0.6745

        if(Scale == 0) {
            convi <- 0
            method.exit <- TRUE
            warning(status <- "could not compute scale of residuals")
        }
        else {
            w <- psi(resid/Scale)
            if (!is.null(weights))
                w <- w * weights

            data$w <- sqrt(w)
            if (missing(control))
                res <-  nls(form, data = data, start = start, algorithm = algorithm,
                             trace = trace, na.action = na.action)
            else
                res <-  nls(form, data = data, start = start, algorithm = algorithm,
                             trace = trace, na.action = na.action, control = control)

            coef <- coefficients(res) ##- same sequence as in start! Ok for test.vec
            ##- attention: coef <- coefficients(res) needed since coef could be
            ##- test.vec

            start <- coef
            resid <- - residuals(res)/sqrt(w) ##- resid(res) == -(y - f(x)) sqrt(w)
            convi <- irls.delta(previous, get(test.vec))
        }
        converged <- convi <= acc
        if (converged)
            break
    }

    if(!converged & !method.exit)
        warning(status <- paste("failed to converge in", maxit, "steps"))

    if(!is.null(weights)) {
        tmp <- (weights != 0)
        w[tmp] <- w[tmp]/weights[tmp]
    }

    ##- return the fit (delete some res$.. to avoid misunderstanding for ex. for
    ##- the residuals)
    res$call         <- match.call()
    res$formula      <- oform
    res$new.formula  <- form
    res$coefficients <- coef

    res$residuals        <- resid
    ##-   names(res$residuals) <- rownames(data)

    ##- fitted values
    y <- eval(as.expression(attr(terms(oform), "variables")[[2]]), data)
    res$fitted.values <- y - res$residuals
    attr(res$fitted.values,"label") <- "Fitted values"
    ##-   names(res$fitted.values) <- rownames(data)

    res$Scale  <- Scale
    res$w      <- w
    res$status <- status
    res$psi    <- psi
    res$m      <- NULL
    res$dataClasses <- NULL

    class(res) <- c("nls", "nls.robust")

    res
}

