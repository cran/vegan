"ordiellipse" <-
function (ord, groups, display = "sites", kind = c("sd", "se"), 
    conf, draw=c("lines","polygon"), w = weights(ord, display), ...) 
{
    if (!require(ellipse)) 
        stop("Requires package `ellipse' (from CRAN)")
    kind <- match.arg(kind)
    draw <- match.arg(draw)
    pts <- scores(ord, display = display, ...)
    if (is.null(w)) 
        w <- rep(1, nrow(pts))
    out <- seq(along = groups)
    inds <- names(table(groups))
    for (is in inds) {
        gr <- out[groups == is]
        if (length(gr) > 2) {
            X <- pts[gr, ]
            W <- w[gr]
            mat <- cov.wt(X, W)
            if (kind == "se") 
                mat$cov <- mat$cov/mat$n.obs
            if (missing(conf)) 
                t <- 1
            else t <- sqrt(qchisq(conf, 2))
            if (draw == "lines") 
               lines(ellipse(mat$cov, centre = mat$center, t = t), ...)
            else {
               xy <- ellipse(mat$cov, center=mat$center, t=t)
               polygon(xy[,1] + mat$center[1], xy[,2] + mat$center[2], ...)
            }
        }
    }
    invisible()
}
