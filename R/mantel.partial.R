"mantel.partial" <-
  function (xdis, ydis, zdis, method = "pearson", permutations = 999, 
            strata, na.rm = FALSE) 
{
    part.cor <- function(rxy, rxz, ryz) {
        (rxy - rxz * ryz)/sqrt(1-rxz*rxz)/sqrt(1-ryz*ryz)
    }
    xdis <- as.dist(xdis)
    ydis <- as.vector(as.dist(ydis))
    zdis <- as.vector(as.dist(zdis))
    ## Handle missing values
    if (na.rm)
        use <- "complete.obs"
    else
        use <- "all.obs"
    rxy <- cor(as.vector(xdis), ydis, method = method, use = use)
    rxz <- cor(as.vector(xdis), zdis, method = method, use = use)
    ryz <- cor(ydis, zdis, method = method, use = use)
    variant <- match.arg(method, eval(formals(cor)$method))
    variant <- switch(variant,
                      pearson = "Pearson's product-moment correlation",
                      kendall = "Kendall's rank correlation tau",
                      spearman = "Spearman's rank correlation rho",
                      variant)
    statistic <- part.cor(rxy, rxz, ryz)
    if (permutations) {
        N <- attr(xdis, "Size")
        perm <- rep(0, permutations)
        xmat <- as.matrix(xdis)
        asdist <- row(xmat) > col(xmat)
        for (i in 1:permutations) {
            take <- permuted.index(N, strata)
            permvec <- (xmat[take, take])[asdist]
            rxy <- cor(permvec, ydis, method = method, use = use)
            rxz <- cor(permvec, zdis, method = method, use = use)
            perm[i] <- part.cor(rxy, rxz, ryz)
        }
        signif <- (sum(perm >= statistic)+1)/(permutations + 1)
    }
    else {
        signif <- NA
        perm <- NULL
    }
    res <- list(call = match.call(), method = variant, statistic = statistic, 
                signif = signif, perm = perm, permutations = permutations)
    if (!missing(strata)) {
        res$strata <- deparse(substitute(strata))
        res$stratum.values <- strata
    }
    class(res) <- c("mantel.partial", "mantel")
    res
}

