"vectorfit" <-
  function (X, P, permutations = 0, strata) 
{
  X <- scores(X, display="sites")
  X <- scale(X, scale = FALSE)
  P <- as.matrix(P)
  nc <- ncol(X)
  Q <- qr(X)
  H <- qr.fitted(Q, P)
  heads <- qr.coef(Q, P)
  r <- diag(cor(H, P)^2)
  heads <- decostand(heads, "norm", 2)
  heads <- t(heads)
  if (is.null(colnames(X))) 
    colnames(heads) <- paste("Dim", 1:nc, sep = "")
  else colnames(heads) <- colnames(X)
  if (permutations) {
    permstore <- matrix(nrow = permutations, ncol = ncol(P))
    for (i in 1:permutations) {
      indx <- permuted.index(nrow(P), strata)
      take <- P[indx, ]
      Hperm <- qr.fitted(Q, take)
      permstore[i, ] <- diag(cor(Hperm, take))^2
    }
    permstore <- sweep(permstore, 2, r, ">")
    pvals <- apply(permstore, 2, sum)/permutations
  }
  else pvals <- NULL
  sol <- list(arrows = heads, r = r, permutations = permutations, 
              pvals = pvals)
  if (!missing(strata)) {
    sol$strata <- deparse(substitute(strata))
    sol$stratum.values <- strata
  }
  class(sol) <- "vectorfit"
  sol
}
