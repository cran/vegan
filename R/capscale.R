"capscale" <-
    function (formula, data, distance = "euclidean", comm = NULL, 
              ...) 
{
    if (!inherits(formula, "formula")) 
        stop("Needs a model formula")
    if (missing(data)) {
        data <- parent.frame()
    }
    X <- formula[[2]]
    X <- eval(X)
    if (!inherits(X, "dist")) {
        comm <- X
        X <- vegdist(X, method = distance)
    }
    inertia <- attr(X, "method")
    inertia <- paste(toupper(substr(inertia, 1, 1)), substr(inertia, 
                                                            2, 256), sep = "")
    inertia <- paste("squared", inertia, "distance")
    k <- attr(X, "Size") - 1
    if (max(X) >= 4 + .Machine$double.eps) {
        inertia <- paste("mean", inertia)
        adjust <- 1
    }
    else {
        adjust <- k
    }
    X <- cmdscale(X, k = k, eig = TRUE)
    X$points <- adjust * X$points
    neig <- min(which(X$eig < 0) - 1, k)
    sol <- X$points[, 1:neig]
    fla <- update(formula, sol ~ .)
    d <- ordiParseFormula(fla, data)
    sol <- rda.default(d$X, d$Y, d$Z, ...)
    sol$tot.chi <- sol$tot.chi
    if (!is.null(sol$CCA)) {
        colnames(sol$CCA$u) <- colnames(sol$CCA$biplot) <- names(sol$CCA$eig) <- colnames(sol$CCA$wa) <- colnames(sol$CCA$v) <- paste("CAP", 
                                                                                                                                      1:ncol(sol$CCA$u), sep = "")
    }
    if (!is.null(comm)) {
        comm <- scale(comm, center = TRUE, scale = FALSE)
        if (!is.null(sol$pCCA)) 
            comm <- qr.resid(sol$pCCA$QR, comm)
        if (!is.null(sol$CCA)) {
            sol$CCA$v.eig <- t(comm) %*% sol$CCA$u/sqrt(k)
            sol$CCA$v <- sweep(sol$CCA$v.eig, 2, sqrt(sol$CCA$eig), 
                               "/")
            comm <- qr.resid(sol$CCA$QR, comm)
        }
        if (!is.null(sol$CA)) {
            sol$CA$v.eig <- t(comm) %*% sol$CA$u/sqrt(k)
            sol$CA$v <- sweep(sol$CA$v.eig, 2, sqrt(sol$CA$eig), 
                              "/")
        }
    }
    if (!is.null(sol$CCA)) 
        sol$CCA$centroids <- centroids.cca(sol$CCA$wa, d$modelframe)
    if (!is.null(sol$CCA$alias)) 
        sol$CCA$centroids <- unique(sol$CCA$centroids)
    if (!is.null(sol$CCA$centroids)) {
        rs <- rowSums(sol$CCA$centroids^2)
        sol$CCA$centroids <- sol$CCA$centroids[rs > 1e-04, , 
                                               drop = FALSE]
    }
    sol$call <- match.call()
    sol$terms <- terms(formula, "Condition", data = data)
    sol$terminfo <- ordiTerminfo(d, data)
    sol$call$formula <- formula(d$terms, width.cutoff = 500)
    sol$call$formula[[2]] <- formula[[2]]
    sol$method <- "capscale"
    sol$inertia <- inertia
    class(sol) <- c("capscale", class(sol))
    sol
}
