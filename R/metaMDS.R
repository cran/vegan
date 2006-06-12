"metaMDS" <-
function (comm, distance = "bray", k = 2, trymax = 20, autotransform = TRUE, 
    noshare = 0.1, expand = TRUE, trace = 1, plot = FALSE, previous.best, 
    ...) 
{
    commname <- deparse(substitute(comm))
    dis <- metaMDSdist(comm, distance = distance, autotransform = autotransform, 
        noshare = noshare, trace = trace, commname = commname, 
        ...)
    if (missing(previous.best)) 
        previous.best <- NULL
    out <- metaMDSiter(dis, k = k, trymax = trymax, trace = trace, 
        plot = plot, previous.best = previous.best, ...)
    maxdis <- attr(dis, "maxdis")
    if (is.null(maxdis)) 
        maxdis <- max(dis)
    points <- postMDS(out$points, dis, plot = max(0, plot - 1), 
        halfchange = (maxdis < 1.1), ...)
    if (is.null(rownames(points))) 
        rownames(points) <- rownames(comm)
    wa <- wascores(points, comm, expand = expand)
    out$points <- points
    out$species <- wa
    out$call <- match.call()
    class(out) <- "metaMDS"
    out
}

