.First.lib <- function(lib, pkg) {
    ## This assumes  "gv"  in your path --- ideally this would be configured!
    options("eps_view" =
            "gv -watch -geometry -0+0 -magstep -2 -media BBox -noantialias")
}
