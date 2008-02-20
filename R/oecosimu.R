`oecosimu` <-
    function(comm, nestfun, method, nsimul=99,
             burnin=0, thin=1, ...)
{
    nestfun <- match.fun(nestfun)
    method <- match.arg(method, c("r00", "r0", "r1", "r2", "c0",
                                  "swap", "tswap", "backtrack",
                                  "quasiswap"))
    ind <- nestfun(comm, ...)
    simind <- numeric(nsimul)
    comm <- ifelse(comm > 0, 1, 0)
    if (method %in% c("swap", "tswap")){
        checkbrd <- 1
        if (method == "tswap") {
            checkbrd <- sum(designdist(comm, "(J-A)*(J-B)", "binary"))
            M <- ncol(comm)
            N <- nrow(comm)
            checkbrd <- M*(M-1)*N*(N-1)/4/checkbrd
            thin <- round(thin*checkbrd)
        }
        attr(simind, "thin") <- thin
        attr(simind, "burnin") <- burnin
        x <- comm
        if (burnin > 0)
            for(i in 1:burnin)
                x <- commsimulator(x, method= method, thin = round(checkbrd))
        for(i in 1:nsimul) {
            x <- commsimulator(x, method = method, thin = thin)
            simind[i] <- nestfun(x, ...)$statistic
        }
    }
    else {
        for (i in 1:nsimul) {
            x <- commsimulator(comm, method=method)
            simind[i] <- nestfun(x,...)$statistic
        }
    }
    z <- (ind$statistic - mean(simind))/sd(simind)
    p <- 2*min(sum(ind$statistic > simind), sum(ind$statistic < simind))
    p <- (p + 1)/(nsimul + 1)
    if (is.null(names(ind$statistic)))
        names(ind$statistic) <- "Statistic"
    ind$oecosimu <- list(z = z, pval = p, simulated=simind, method=method,
                         statistic = ind$statistic)
    class(ind) <- c("oecosimu", class(ind))
    ind
}
