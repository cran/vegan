"wascores" <-
    function (x, w, expand = FALSE) 
{
    x <- as.matrix(x)
    w <- as.matrix(w)
    nc <- ncol(x)
    nr <- ncol(w)
    wa <- matrix(NA, nrow = nr, ncol = nc)
    colnames(wa) <- colnames(x)
    rownames(wa) <- colnames(w)
    for (i in 1:nr) {
        wa[i, ] <- apply(x, 2, weighted.mean, w = w[, i])
    }
    if (expand) {
        x.w <- rowSums(w)
        wa.w <- colSums(w)
        x.cov <- cov.wt(x, x.w)
        wa.cov <- cov.wt(wa, wa.w)
        mul <- sqrt(diag(x.cov$cov)/diag(wa.cov$cov))
        wa <- sweep(wa, 2, wa.cov$center, "-")
        wa <- sweep(wa, 2, mul, "*")
        wa <- sweep(wa, 2, wa.cov$center, "+")
    }
    wa
}
