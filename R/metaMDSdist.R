"metaMDSdist" <-
    function (comm, distance = "bray", autotransform = TRUE, 
              noshare = 0.1, trace = 1, commname, ...) 
{
    formals(vegdist) <- c(formals(vegdist), alist(... = ))
    formals(stepacross) <- c(formals(stepacross), alist(...=))
    if (missing(commname))
        commname <- deparse(substitute(comm))
    xam <- max(comm)
    if (autotransform && xam > 50) {
        comm <- sqrt(comm)
        commname <- paste("sqrt(", commname, ")", sep = "")
        if (trace) 
            cat("Square root transformation\n")
    }
    if (autotransform && xam > 9) {
        comm <- wisconsin(comm)
        commname <- paste("wisconsin(", commname, ")", sep = "")
        if (trace) 
            cat("Wisconsin double standardization\n")
    }
    dis <- vegdist(comm, method = distance, ...)
    maxdis <- max(dis)
    if (sum(no.shared(comm))/length(dis) > noshare) {
        if (trace) 
            cat("Using step-across dissimilarities:\n")
        dis <- stepacross(dis, trace = trace, ...)
    }
    attr(dis, "maxdis") <- maxdis
    attr(dis, "commname") <- commname
    dis
}

