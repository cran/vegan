"protest" <-
    function (X, Y, scores = "sites", permutations = 999, strata, ...)
{
    X <- scores(X, display = scores, ...)
    Y <- scores(Y, display = scores, ...)
    ## Centre and normalize X & Y here so that the permutations will
    ## be faster
    X <- scale(X, scale = FALSE)
    Y <- scale(Y, scale = FALSE)
    X <- X/sqrt(sum(X^2))
    Y <- Y/sqrt(sum(Y^2))
    ## Transformed X and Y will yield symmetric procrustes() and we
    ## need not specify that in the call (but we set it symmetric
    ## after the call).
    sol <- procrustes(X, Y, symmetric = FALSE)
    sol$symmetric <- TRUE
    sol$t0 <- sqrt(1 - sol$ss)
    N <- nrow(X)
    perm <- rep(0, permutations)
    for (i in 1:permutations) {
        take <- permuted.index(N, strata)
        ## avoid overhead of procrustes() and only evaluate the
        ## statistic by svd (hand crafted from r2388 of the devel
        ## branch).
        perm[i] <- sum(svd(crossprod(X, Y[take,]), nv = 0, nu = 0)$d)
    }
    Pval <- (sum(perm >= sol$t0) + 1)/(permutations + 1)
    if (!missing(strata)) {
        strata <- deparse(substitute(strata))
        s.val <- strata
    }
    else {
        strata <- NULL
        s.val <- NULL
    }
    sol$t <- perm
    sol$signif <- Pval
    sol$permutations <- permutations
    sol$strata <- strata
    sol$stratum.values <- s.val
    sol$call <- match.call()
    class(sol) <- c("protest", "procrustes")
    sol
}
