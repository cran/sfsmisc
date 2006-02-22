## From: Christian Sangiorgio <sangior@stat.math.ethz.ch>
## To: maechler@stat.math.ethz.ch
## Subject: rnls
## Date: Fri, 14 Jan 2005 20:19:03 +0100
##   and
## Date: Mon, 17 Jan 2005 09:29:06 +0100

## beim Lesen der rnls Funktion habe ich noch einen Fehler
## gefunden und zwar im Fall wo die Gewichte ('weights') geben
## sind. Früher hat man die  variable 'w' nach der robustes Schleife
## definiert und mit ihr die resid und fitted definiert... ohne
## berücksichtigung der externen  Gewichte.

## MM:                      ~~~~~~
## -- to DO: summary, print method for this class (and fitted, residuals, ...)
##                          ~~~~~~
## "rlm" has quite sophisticated ones, see
 ##	/usr/local/app/R/R_local/src/VR/MASS/R/rlm.R
## but the "nls" ones that are currently used, have too, see
##	/u/maechler/R/D/r-devel/R/src/library/stats/R/nls.R

rnls <- function(formula, data, start, weights = NULL, na.action = na.fail,
                 psi = MASS::psi.huber, test.vec = c("resid", "coef", "w"),
                 maxit = 20, acc = 1e-06, algorithm = "default",
                 control = nls.control(), trace = FALSE, ...)
{
    ## Purpose:
    ##  Robust parameters estimation in the nonlinear model. The fitting is
    ##  done by iterated reweighted least squares (IWLS) as in rlm() of the
    ##  package MASS. In addition, see also 'nls'.
    ##
    ## --> see the help file,  ?rnls  (or ../man/rnls.Rd in the source)
    ## -------------------------------------------------------------------------

    cat("rnls() in package 'sfsmisc' is deprecated (and will be removed)",
	"\n do use nlrob() from the 'robustbase' package instead!\n")

    ##- some checks
    mf <- match.call() # << and more as in nls()  [FIXME or drop]
    formula <- as.formula(formula)
    if (length(formula) != 3)
	stop("'formula' should be a formula of the type 'y  ~ f(x, alpha)'")
    varNames <- all.vars(formula)
    test.vec <- match.arg(test.vec)
    dataName <- substitute(data)
    data <- as.data.frame(data)

    ## FIXME:  nls() allows  a missing 'start';  we don't :
    if(length(pnames <- names(start)) != length(start))
        stop("'start' must be fully named (list or numeric vector)")
    if (!((is.list(start)   && all(sapply(start,is.numeric))) ||
          (is.vector(start) && is.numeric(start))) ||
        any(is.na(match(pnames, varNames))))
        stop("'start' must be a list or numeric vector named with parameters in 'formula'")

    if ("w" %in% varNames || "w" %in% pnames || "w" %in% names(data))
        stop("Do not use 'w' as a variable name or as a parameter name")

    if (!is.null(weights)) {
        if (length(weights) != nrow(data))
            stop("'length(weights)' must equal the number of observations")
        if (any(weights < 0) || any(is.na(weights)))
            stop("'weights' must be nonnegative and not contain NAs")
    }

    ## if (any(is.na(data)) & options("na.action")$na.action == "na.omit")
    ##   stop("if NAs are present, use 'na.exclude' to preserve the residuals length")

    irls.delta <- function(old, new)
        sqrt(sum((old - new)^2, na.rm = TRUE) / max(1e-20, sum(old^2,na.rm = TRUE)))

    ##- initialize testvec  and update formula with robust weights
    coef <- start
    fit <- eval(formula[[3]], c(as.list(data), start))
    y <- eval(formula[[2]], as.list(data))
    resid <- y - fit
    w <- rep(1, nrow(data))
    if (!is.null(weights))
        w <- w * weights

    oform <- formula
### FIXME:  nls() in R 2.3.0. *does* now work with weights !

    formula <- as.formula(substitute(~(LHS-RHS) * w, list(LHS = formula[[2]],
							  RHS = formula[[3]])))

    ##- robust loop:
    converged <- FALSE
    status <- "converged"
    method.exit <- FALSE

    for(iiter in 1:maxit) {
        if(trace) cat("robust iteration", iiter, "\n")
        previous <- get(test.vec)
        Scale <- median(abs(resid), na.rm = TRUE)/0.6745

        if(Scale == 0) {
            convi <- 0
            method.exit <- TRUE
            warning(status <- "could not compute scale of residuals")
            ## FIXME : rather use a "better" Scale in this case, e.g.,
            ## -----   Scale <- min(abs(resid)[resid != 0])
        }
        else {
            w <- psi(resid/Scale, ...)
            if (!is.null(weights))
                w <- w * weights

            data$w <- sqrt(w)
            out <- nls(formula, data = data, start = start, algorithm = algorithm,
                       trace = trace, na.action = na.action, control = control)

            ## same sequence as in start! Ok for test.vec:
            coef <- coefficients(out)
            ## above coef <- coefficients(out) needed since coef could be
            ## test.vec

            start <- coef
            resid <- - residuals(out)/sqrt(w)## == - (y - f(x)) sqrt(w)
            convi <- irls.delta(previous, get(test.vec))
        }
        converged <- convi <= acc
        if (converged)
            break
    }

    if(!converged && !method.exit)
        warning(status <- paste("failed to converge in", maxit, "steps"))

    if(!is.null(weights)) {
        tmp <- weights != 0
        w[tmp] <- w[tmp]/weights[tmp]
    }

    ##- return the fit
    out <- list(m = out$m, call = match.call(), formula = oform,
                new.formula = formula,
		coefficients = coef, ## == coefficients(out)
                residuals = resid) ## == - residuals(out)/sqrt(w) # minus !
    ##ARu: dropped this: 'y <- eval(...)':
    ## y <- eval(as.expression(attr(terms(oform), "variables")[[2]]), data)
    out$fitted.values <- y - out$residuals
    attr(out$fitted.values, "label") <- "Fitted values"
    ##-   names(out$residuals) <- rownames(data)
    ##-   names(out$fitted.values) <- rownames(data)
    out$Scale <- Scale
    out$w <- w
    out$status <- status
    out$psi <- psi
    out$data <- dataName
    out$dataClasses <- attr(attr(mf, "terms"), "dataClasses")

    class(out) <- c("rnls", "nls")

    out
}

### These are from  Andreas Ruckstuhl, August 2005 :

fitted.rnls <- function(object, ...)
{
    val <- as.vector(object$fitted.values)
    if (!is.null(object$na.action))
	val <- napredict(object$na.action, val)
    attr(val, "label") <- "Fitted values"
    val
}
residuals.rnls <- function (object, ...)
{
   val <- as.vector(object$residuals)
   if (!is.null(object$na.action))
     val <- naresid(object$na.action, val)
   attr(val, "label") <- "Residuals"
   val
}

predict.rnls <- function (object, newdata, ...)
{
    if (missing(newdata))
	return(as.vector(fitted(object)))
    if (!is.null(cl <- object$dataClasses))
	.checkMFClasses(cl, newdata)
    eval(object$formula[[3]], c(as.list(newdata), coef(object)))
}

print.rnls <- function (x, ...)
{
    cat("Robustly fitted nonlinear regression model\n")
    cat("  model: ", deparse(formula(x)), "\n")
    cat("   data: ", deparse(x$data), "\n")
    print(coef(x), ...)
    cat(" status: ", x$status, "\n")
    invisible(x)
}

## NOTE:  coef(<rnls>)  works via  coef.nls() !

## MM: don't know if this is a good idea;  formula.default() would work too
## formula.rnls <- function (x, ...)
##   x$formula

summary.rnls <- function (object, ...)
{
    .NotYetImplemented()
    ## that may be a good start:
    nlsSum <- stats:::summary.nls(object, ...)
    ## but we need to change the call;
    ## additionally show robustness weights, and --- most importantly ---
    ## "Fix the inference" (z-values / sandwich-STD.ERR. ?)
    object
}

