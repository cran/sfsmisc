hatMat <- function(x, trace = FALSE,
                   pred.sm = function(x,y,...)
                   predict(smooth.spline(x,y, ...), x = x)$y,
                   ...)
{
    ## Purpose: Return Hat matrix of a smoother -- very general (but slow)
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  7 Mar 2001, 11:12
    n <- NROW(x)
    y <- pred.sm(x, numeric(n), ...)
    if(!is.numeric(y) || length(y) !=n)
        stop("`pred.sm' does not return a numeric length n vector")
    H <- if(trace) 0 else matrix(as.numeric(NA), n,n)
    for (i in 1:n) {
        y <- numeric(n) ; y[i] <- 1
        y <- pred.sm(x, y, ...)
        if(trace) H <- H + y[i] else H[,i] <- y
    }
    H
}

