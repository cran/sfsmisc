## This assumes  "gv"  in your path --- ideally this would be configured!
options("eps_view" =
        "gv -watch -geometry -0+0 -magstep -2 -media BBox -noantialias")

## Needed before R version 1.3.0 :
if(!exists("is.unsorted", mode = "function"))
    is.unsorted <- function(x, na.rm = FALSE) {
        if(!na.rm && any(is.na(x)))
            return(as.logical(NA))
        ## else
        if(na.rm && any(ii <- is.na(x)))
            x <- x[!ii]

        (length(x) > 1) && any(diff(x) < 0)
    }
