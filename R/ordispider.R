"ordispider" <-
    function(ord, groups, display="sites", w = weights(ord, display), ...)
{
    if (inherits(ord, "cca") && missing(groups)) {
        lc <- scores(ord, display = "lc", ...)
        wa <- scores(ord, display = "wa", ...)
        segments(lc[,1], lc[,2], wa[,1], wa[,2], ...)
        return(invisible())
    }
    pts <- scores(ord, display=display, ...)
    if (is.null(w))
        w <- rep(1, nrow(pts))
    out <- seq(along=groups)
    inds <- names(table(groups))
    for (is in inds) {
        gr <- out[groups == is]
        if (length(gr) > 1) {
            X <- pts[gr, ]
            W <- w[gr]
            ave <- apply(X, 2, weighted.mean, w=W)
            segments(ave[1], ave[2],X[,1], X[,2], ...)
        }
    }
    invisible()
}
