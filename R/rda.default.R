"rda.default" <-
    function (X, Y, Z, scale = FALSE, ...) 
{
    CCA <- NULL
    pCCA <- NULL
    CA <- NULL
    X <- as.matrix(X)
    NR <- nrow(X) - 1
    Xbar <- scale(X, center = TRUE, scale = scale)
    tot.chi <- sum(svd(Xbar, nu = 0, nv = 0)$d^2)/NR
    if (!missing(Z) && !is.null(Z)) {
        Z <- as.matrix(Z)
        Z.r <- scale(Z, center = TRUE, scale = FALSE)
        Q <- qr(Z.r)
        Z <- qr.fitted(Q, Xbar)
        tmp <- sum(svd(Z, nu = 0, nv = 0)$d^2)/NR
        pCCA <- list(rank = Q$rank, tot.chi = tmp, QR = Q, Fit = Z)
        Xbar <- qr.resid(Q, Xbar)
    }
    else Z.r <- NULL
    if (!missing(Y) && !is.null(Y)) {
        Y <- as.matrix(Y)
        rawmat <- Y
        Y.r <- scale(Y, center = TRUE, scale = FALSE)
        Q <- qr(cbind(Y.r, Z.r))
        if (is.null(pCCA)) 
            rank <- Q$rank
        else rank <- Q$rank - pCCA$rank
        Y <- qr.fitted(Q, Xbar)
        sol <- svd(Y)
        rank <- min(rank, length(sol$d))
        sol$d <- sol$d/sqrt(NR)
        ax.names <- paste("RDA", 1:length(sol$d), sep = "")
        colnames(sol$u) <- ax.names
        colnames(sol$v) <- ax.names
        names(sol$d) <- ax.names
        rownames(sol$u) <- rownames(X)
        rownames(sol$v) <- colnames(X)
        CCA <- list(eig = sol$d[1:rank]^2)
        CCA$u <- as.matrix(sol$u)[, 1:rank, drop = FALSE]
        CCA$v <- as.matrix(sol$v)[, 1:rank, drop = FALSE]
        CCA$u.eig <- sweep(as.matrix(CCA$u), 2, sol$d[1:rank], 
                           "*")
        CCA$v.eig <- sweep(as.matrix(CCA$v), 2, sol$d[1:rank], 
                           "*")
        CCA$wa.eig <- Xbar %*% sol$v[, 1:rank, drop = FALSE]
        CCA$wa.eig <- CCA$wa.eig/sqrt(NR)
        CCA$wa <- sweep(CCA$wa.eig, 2, 1/sol$d[1:rank], "*")
        CCA$biplot <- cor(as.matrix(Y.r), sol$u[, 1:rank, drop = FALSE])
        CCA$rank <- rank
        CCA$tot.chi <- sum(CCA$eig)
        CCA$QR <- Q
        CCA$Xbar <- Xbar
        Xbar <- qr.resid(Q, Xbar)
    }
    Q <- qr(Xbar)
    if (Q$rank > 0) {
        sol <- svd(Xbar)
        sol$d <- sol$d/sqrt(NR)
        ax.names <- paste("PC", 1:length(sol$d), sep = "")
        colnames(sol$u) <- ax.names
        colnames(sol$v) <- ax.names
        names(sol$d) <- ax.names
        rownames(sol$u) <- rownames(X)
        rownames(sol$v) <- colnames(X)
        CA <- list(eig = (sol$d[1:Q$rank]^2))
        CA$u <- as.matrix(sol$u)[, 1:Q$rank, drop = FALSE]
        CA$v <- as.matrix(sol$v)[, 1:Q$rank, drop = FALSE]
        CA$u.eig <- sweep(as.matrix(CA$u), 2, sol$d[1:Q$rank], 
                          "*")
        CA$v.eig <- sweep(as.matrix(CA$v), 2, sol$d[1:Q$rank], 
                          "*")
        CA$rank <- Q$rank
        CA$tot.chi <- sum(CA$eig)
        CA$Xbar <- Xbar
    }
    call <- match.call()
    call[[1]] <- as.name("rda")
    sol <- list(call = call, grand.total = NA, rowsum = NA, colsum = NA, 
                tot.chi = tot.chi, pCCA = pCCA, CCA = CCA, CA = CA)
    sol$method <- "rda"
    sol$inertia <- if (scale) "correlations" else "variance"
    class(sol) <- c("rda", "cca")
    sol
}
