"ordihull" <-
function (ord, groups, display = "sites", draw=c("lines","polygon"), ...) 
{
    draw <- match.arg(draw)
    pts <- scores(ord, display = display, ...)
    out <- seq(along = groups)
    inds <- names(table(groups))
    for (is in inds) {
        gr <- out[groups == is]
        if (length(gr) > 1) {
            X <- pts[gr, ]
            hpts <- chull(X)
            hpts <- c(hpts, hpts[1])
            if (draw == "lines")
               lines(X[hpts, ], ...)
            else
               polygon(X[hpts, ], ...)
        }
    }
    invisible()
}
