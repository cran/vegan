"ordiarrows" <-
function (ord, groups, levels, replicates, display = "sites", 
    ...) 
{
    pts <- scores(ord, display = display, ...)
    npoints <- nrow(pts)
    if (missing(groups)) 
        groups <- gl(levels, replicates, npoints)
    out <- seq(along = groups)
    inds <- names(table(groups))
    for (is in inds) {
        gr <- out[groups == is]
        if (length(gr) > 1) {
            X <- pts[gr, , drop=FALSE]
            X0 <- X[-nrow(X), , drop=FALSE]
            X1 <- X[-1, , drop=FALSE]
            arrows(X0[, 1], X0[, 2], X1[, 1], X1[, 2], ...)
        }
    }
    invisible()
}
