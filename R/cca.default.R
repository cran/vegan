"cca.default" <-
function (X, Y, Z) 
{
  CCA <- NULL
  pCCA <- NULL
  CA <- NULL
  weight.centre <- function(x, w) {
     w.c <- apply(x, 2, weighted.mean, w=w)
     x <- sweep(x, 2, w.c, "-")
     x <- sweep(x, 1, sqrt(w), "*")
     x
  }
  X <- as.matrix(X)
  gran.tot <- sum(X)
  X <- X/gran.tot
  rowsum <- apply(X, 1, sum)
  colsum <- apply(X, 2, sum)
  rc <- outer(rowsum, colsum)
  Xbar <- (X - rc)/sqrt(rc)
  tot.chi <- sum(svd(Xbar, nu=0, nv=0)$d^2)
  if (!missing(Z)) {
    Z <- as.matrix(Z)
    Z.r <- weight.centre(Z, rowsum)
    Q <- qr(Z.r)
    Z <- qr.fitted(Q, Xbar)
    tmp <- sum(svd(Z, nu=0, nv=0)$d^2)
    pCCA <- list(rank = Q$rank, tot.chi = tmp, QR = Q,  Fit = Z)
    Xbar <- qr.resid(Q, Xbar)
  } else Z.r <- NULL
  if (!missing(Y)) {
    Y <- as.matrix(Y)
    rawmat <- Y
    Y.r <- weight.centre(Y, rowsum)
    Q <- qr(cbind(Y.r,Z.r))
    if(is.null(pCCA)) rank <- Q$rank
    else rank <- Q$rank - pCCA$rank
    Y <- qr.fitted(Q, Xbar)
    sol <- svd(Y)
    ax.names <- paste("CCA",1:length(sol$d), sep="")
    colnames(sol$u) <- ax.names
    colnames(sol$v) <- ax.names
    names(sol$d) <- ax.names
    rownames(sol$u) <- rownames(X)
    rownames(sol$v) <- colnames(X)
    CCA <- list(eig = sol$d[1:rank]^2)
    CCA$u <- sweep(as.matrix(sol$u[,1:rank]), 1, 1/sqrt(rowsum), "*")
    CCA$v <- sweep(as.matrix(sol$v[,1:rank]), 1, 1/sqrt(colsum), "*")
    CCA$u.eig <- sweep(CCA$u, 2, sol$d[1:rank], "*")
    CCA$v.eig <- sweep(CCA$v, 2, sol$d[1:rank], "*")
    CCA$wa.eig <- sweep(Xbar %*% sol$v[,1:rank], 1, 1/sqrt(rowsum), "*")
    CCA$wa <- sweep(CCA$wa.eig, 2, 1/sol$d[1:rank], "*")
    CCA$biplot <- cor(Y.r, sol$u[,1:rank])
    CCA$rank <- rank
    CCA$tot.chi <- sum(CCA$eig)
    CCA$QR <- Q
    CCA$Xbar <- Xbar
    Xbar <- qr.resid(Q, Xbar)
  }
  Q <- qr(Xbar)
  if(Q$rank > 0) {
    sol <- svd(Xbar)
    ax.names <- paste("CA",1:length(sol$d), sep="")
    colnames(sol$u) <- ax.names
    colnames(sol$v) <- ax.names
    names(sol$d) <- ax.names
    rownames(sol$u) <- rownames(X)
    rownames(sol$v) <- colnames(X)
    CA <- list(eig = sol$d[1:Q$rank]^2)
    CA$u <- sweep(as.matrix(sol$u[,1:Q$rank]), 1, 1/sqrt(rowsum), "*")
    CA$v <- sweep(as.matrix(sol$v[,1:Q$rank]), 1, 1/sqrt(colsum), "*")
    CA$u.eig <- sweep(CA$u, 2, sol$d[1:Q$rank], "*")
    CA$v.eig <- sweep(CA$v, 2, sol$d[1:Q$rank], "*")
    CA$rank <- Q$rank
    CA$tot.chi <- sum(CA$eig)
    CA$Xbar <- Xbar
  }
  Call <- match.call()
  Call[[1]] <- as.name("cca")
  sol <- list(Call = Call, grand.total = gran.tot,
              rowsum = rowsum, colsum = colsum, tot.chi = tot.chi, 
              pCCA = pCCA, CCA = CCA, CA=CA)
  class(sol) <- "cca"
  sol
}
