"metaMDS" <-
    function (comm, distance = "bray", k = 2, trymax = 20, autotransform = TRUE, 
              noshare = 0.1, expand = TRUE, trace = 1, plot = FALSE, previous.best, ...) 
{
    EPS <- 0.05
    RESLIM <- 0.01
    RMSELIM <- 0.005
    SOL <- FALSE
    converged <- FALSE
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
    isotrace <- max(0, trace - 1)
    if (!missing(previous.best) && !is.null(previous.best)) {
        s0 <- previous.best
        if (trace)
            cat("Starting from a previous solution\n")
    }
    else 
        s0 <- isoMDS(dis, k = k, trace = isotrace)
    if (trace) 
        cat("Run 0 stress", s0$stress, "\n")
    tries <- 1
    repeat {
        stry <- isoMDS(dis, initMDS(dis, k = k), k = k, maxit = 200, 
                       tol = 1e-07, trace = isotrace)
        if (trace) {
            cat("Run", tries, "stress", stry$stress, "\n")
        }
        if ((s0$stress - stry$stress) > -EPS) {
            pro <- procrustes(s0, stry, symmetric = TRUE)
            if (plot && k > 1) 
                plot(pro)
            if (stry$stress < s0$stress) {
                s0 <- stry
                if (trace) 
                    cat("... New best solution\n")
            }
            summ <- summary(pro)
            if (trace) 
                cat("... rmse", summ$rmse, "  max residual", 
                    max(summ$resid), "\n")
            if (summ$rmse < RMSELIM && max(summ$resid) < RESLIM) {
                if (trace) 
                    cat("*** Solution reached\n\n")
                converged <- TRUE
                break
            }
        }
        if (tries >= trymax) 
            break
        tries <- tries + 1
    }
    s0 <- postMDS(s0, dis, plot = max(0, plot-1), halfchange = (maxdis < 1.1))
    wa <- wascores(s0$points, comm, expand = expand)
    out <- list(points = s0$points, species = wa, dims = k, stress = s0$stress, 
                data = commname, distance = attr(dis, "method"), converged = converged, 
                tries = tries, call = match.call())
    class(out) <- "metaMDS"
    out
}
