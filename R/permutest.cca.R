"permutest.cca" <-
function (x, permutations = 100, model = c("reduced", "full"), strata) 
{
    model <- match.arg(model)
    Chi.z <- x$CCA$tot.chi
    Chi.xz <- x$CA$tot.chi
    q <- x$CCA$rank
    r <- x$CA$rank
    F.0 <- (Chi.z/q)/(Chi.xz/r)
    F.perm <- rep(0, permutations)
    if (!is.null(x$pCCA)) {
        Y.Z <- x$pCCA$Fit
        QZ <- x$pCCA$QR
    }
    if (model == "reduced") 
        E <- x$CCA$Xbar
    else E <- x$CA$Xbar
    N <- nrow(E)
    Q <- x$CCA$QR
    for (i in 1:permutations) {
        take <- permuted.index(N, strata)
        Y <- E[take, ]
        if (!is.null(x$pCCA)) {
            Y <- Y.Z + Y
            Y <- qr.resid(QZ, Y)
        }
        cca.ev <- sum(diag(crossprod(qr.fitted(Q, Y))))
        ca.ev <- sum(diag(crossprod(qr.resid(Q, Y))))
        F.perm[i] <- (cca.ev/q)/(ca.ev/r)
    }
    sol <- list(Call = x$Call, model = model, F.0 = F.0, F.perm = F.perm, 
        nperm = permutations)
    if(!missing(strata)) {
        sol$strata <- deparse(substitute(strata)) 
        sol$stratum.values <- strata
    }
    class(sol) <- "permutest.cca"
    sol
}
