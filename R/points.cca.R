"points.cca" <-
    function (x, display = "sites", choices = c(1, 2), scaling = 2, 
              arrow.mul = 1, head.arrow = 0.05, ...) 
{
    formals(arrows) <- c(formals(arrows), alist(... = ))
    if (length(display) > 1) 
        stop("Only one `display' item can be added in one command.")
    pts <- scores(x, choices = choices, display = display, scaling = scaling)
    if (display == "cn") {
        cnam <- rownames(pts)
        points(pts, ...)
        pts <- scores(x, choices = choices, display = "bp", scaling = scaling)
        bnam <- rownames(pts)
        pts <- pts[!(bnam %in% cnam), , drop = FALSE]
        if (nrow(pts) == 0) 
            return(invisible())
        else display <- "bp"
    }
    if (display == "bp") {
        pts <- pts * arrow.mul
        arrows(0, 0, pts[, 1], pts[, 2], length = head.arrow, 
               ...)
        pts <- pts * 1.1
        axis(3, at = c(-arrow.mul, 0, arrow.mul), labels = rep("", 
                                                  3))
        axis(4, at = c(-arrow.mul, 0, arrow.mul), labels = c(-1, 
                                                  0, 1))
        return(invisible())
    }
    points(pts, ...)
    invisible()
}
