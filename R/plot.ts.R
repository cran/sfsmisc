if(paste(R.version$major, R.version$minor, sep=".") < 1.2) {
###---- This is a modification of "base"  plot.ts !!!!!
###  1) Modify the xy-plot by allowing suppression of lines / text
###	-> arguments  xy.lines, xy.labels
###  2) ___not yet___: for long series,
###		       do several plots slightly overlapping in time
###
### from /u/maechler/R/r-devel/R/src/library/base/R/ts.R

plot.ts <-
function (x, y = NULL, type = "l", xlim = NULL, ylim = NULL,
	  xlab = "Time", ylab, log = "",
	  col = par("col"), bg = NA,
	  pch = par("pch"), cex = par("cex"),
	  lty = par("lty"), lwd = par("lwd"),
	  axes = TRUE, frame.plot = axes, ann = par("ann"),
	  main = NULL, plot.type = c("multiple", "single"),
	  xy.labels = n <= 150, xy.lines = do.lab, ...)
{
    xlabel <- if (!missing(x)) deparse(substitute(x)) else NULL
    ylabel <- if (!missing(y)) deparse(substitute(y)) else NULL
    plot.type <- match.arg(plot.type)
    if(plot.type == "multiple" && NCOL(x) > 1) {
	m <- match.call()
	m[[1]] <- as.name("plot.mts")
	return(eval(m, parent.frame()))
    }
    x <- as.ts(x)
    if(!is.null(y)) {
	## want ("scatter") plot of y ~ x
	y <- hasTsp(y)
	if(NCOL(x) > 1 || NCOL(y) > 1)
	    stop("scatter plots only for univariate time series")
	if(is.ts(x) && is.ts(y)){
	    xy <- ts.intersect(x, y)
	    xy <- xy.coords(xy[,1], xy[,2], xlabel, ylabel, log)
	} else
	    xy <- xy.coords(x, y, xlabel, ylabel, log)
	xlab <- if (missing(xlab)) xy$xlab else xlab
	ylab <- if (missing(ylab)) xy$ylab else ylab
	xlim <- if (is.null(xlim)) range(xy$x[is.finite(xy$x)]) else xlim
	ylim <- if (is.null(ylim)) range(xy$y[is.finite(xy$y)]) else ylim
	n <- length(xy $ x) #-> default for xy.l(ines|abels)
	if(!is.logical(xy.labels)) {
	    if(!is.character(xy.labels))
		stop("`xy.labels' must be logical or character")
	    do.lab <- TRUE
	} else do.lab <- xy.labels
	    
        ptype <-
            if(do.lab) "n" else if(missing(type)) "p" else type
	plot.default(xy, type = ptype, 
		     xlab = xlab, ylab = ylab,
		     xlim = xlim, ylim = ylim, log = log, col = col, bg = bg,
		     pch = pch, axes = axes, frame.plot = frame.plot,
		     ann = ann, main = main, ...)
	if(do.lab)
	    text(xy, labels =
		 if(is.character(xy.labels)) xy.labels
		 else if(all(tsp(x) == tsp(y))) formatC(time(x), wid = 1)
		 else seq(along = x),
		 col = col, cex = cex)
	if(xy.lines)
	    lines(xy, col = col, lty = lty, lwd = lwd,
                  type = if(do.lab) "c" else "l")
	return(invisible())
    }
    if(missing(ylab)) ylab <- xlabel
    time.x <- time(x)
    if(is.null(xlim)) xlim <- range(time.x)
    if(is.null(ylim)) ylim <- range(x[is.finite(x)])
    plot.new()
    plot.window(xlim, ylim, log, ...)
    if(is.matrix(x)) {
	for(i in 1:ncol(x))
	    lines.default(time.x, x[,i],
			  col = col[(i-1) %% length(col) + 1],
			  lty = lty[(i-1) %% length(lty) + 1],
			  lwd = lwd[(i-1) %% length(lwd) + 1],
			  bg  =	 bg[(i-1) %% length(bg)	 + 1],
			  pch = pch[(i-1) %% length(pch) + 1],
			  type = type)
    }
    else {
	lines.default(time.x, x, col = col[1], bg = bg, lty = lty[1],
		      lwd = lwd[1], pch = pch[1], type = type)
    }
    if (ann)
	title(main = main, xlab = xlab, ylab = ylab, ...)
    if (axes) {
	axis(1, ...)
	axis(2, ...)
    }
    if (frame.plot) box(...)
}


}# only if R <= 1.1
