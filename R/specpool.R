"specpool" <-
function(x, pool)
{
    x <- as.matrix(x)
    if (missing(pool))
        pool <- rep("All", nrow(x))
    out <- seq(1:nrow(x))
    groups <- table(pool)
    inds <- names(groups)
    S <- chao <- jack.1 <- jack.2 <- bootS <- rep(NA, length(inds))
    names(S) <- names(chao) <- names(jack.1) <- names(jack.2) <- names(bootS) <- inds
    for (is in inds) {
        a1 <- a2 <- NA
        gr <- out[pool == is]
        n <- length(gr)
        X <- x[gr, , drop=FALSE]
        freq <- colSums(X > 0)
        p <- freq[freq>0]/n
        S[is] <- sum(freq > 0)
        if (n > 1)
            a1 <- sum(freq == 1)
        if (n > 2)
            a2 <- sum(freq == 2)
        chao[is] <- S[is] + a1*a1/2/a2
        jack.1[is] <- S[is] + a1*(n-1)/n
        jack.2[is] <- S[is] + a1*(2*n-3)/n - a2*(n-2)^2/n/(n-1)
        bootS[is] <- S[is] + sum((1 - p)^n)
    }
    out <- list(Species = S, Chao = chao, Jack.1 = jack.1, Jack.2 = jack.2,
                Boot = bootS, n = as.vector(groups))
    out <- as.data.frame(out)
    attr(out, "pool") <- pool
    out
}
