"vectorfit" <-
  function (X, P) 
{
  X <- as.matrix(X)
  P <- as.matrix(P)
  nc <- ncol(X)
  nr <- ncol(P)
  sol <- matrix(NA, nrow = nr, ncol = nc + 1)
  rownames(sol) <- colnames(P)
  colnames(sol) <- c(paste("head", 1:nc, sep = ""), "r")
  Q <- qr(X)
  for (i in 1:nr) {
    H <- qr.fitted(Q, P[, i])
    heads <- qr.coef(Q, P[, i])
    heads <- decostand(as.matrix(heads), "norm", 2)
    r <- cor(H, P[, i])
    sol[i, 1:nc] <- heads
    sol[i, nc + 1] <- r
  }
  sol
}
