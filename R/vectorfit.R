"vectorfit" <-
  function (X, P) 
{
    X <- as.matrix(X)
    X <- scale(X, scale = FALSE)
    P <- as.matrix(P)
    nc <- ncol(X)
    Q <- qr(X)
    H <- qr.fitted(Q, P)
    heads <- qr.coef(Q,P)
    r <- diag(cor(H,P))  
    heads <- decostand(heads, "norm", 2)
    sol <- cbind(t(heads), r)
    if (is.null(colnames(X)))
        colnames(sol) <- c(paste("Dim", 1:nc, sep = ""), "r")
    else colnames(sol) <- c(colnames(X), "r")
    sol
}
