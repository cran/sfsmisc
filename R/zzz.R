.onLoad <- function(lib, pkg)
{
    ## This assumes  "gv"  in your path --- ideally this would be configured!
    if(.Platform $ OS.type == "unix") {
	cmd <- "gv -watch -geometry -0+0 -magstep -2 -media BBox -noantialias"
	hyphens <-
	    system(paste("gv -h | fgrep watch | head -1",
			 "| sed 's/watch.*//; s/^[\s ]*//'"),
		   intern=TRUE)
	if(hyphens == "--")
            cmd <- sub(" --geometry", " -geometry",
                       sub(" --magstep ", " --scale=",
                           sub(" --media ", " --media=",
                               gsub(" -([a-z])", " --\\1", cmd))))
	options("eps_view" = cmd)
    }
}

## was  .First.lib <- function(lib, pkg) { .. }
