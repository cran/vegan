"factorfit" <-
  function (X, P, permutations = 0, strata) 
{
  GOF <- function(X, A) {
    n <- table(A)
    N <- sum(n) - 1
    n <- n - 1
    totvar <- N * sum(apply(X, 2, var))
    tmp <- apply(X, 2, tapply, A, var)
    invar <- sum(sweep(tmp, 1, n, "*"))
    r <- 1 - invar/totvar
    r
  }
  sol <- NULL
  r <- NULL
  pval <- NULL
  X <- scores(X, display="sites")
  P <- as.data.frame(P)
  for (i in 1:length(P)) {
    tmp <- apply(X, 2, tapply, P[[i]], mean)
    nam <- rownames(tmp)
    nam <- paste(names(P)[i], nam, sep = "")
    rownames(tmp) <- nam
    sol <- rbind(sol, tmp)
    r.this <- GOF(X, P[[i]])
    r <- c(r, r.this)
    if (permutations) {
      A <- P[[i]]
      tmp <- rep(NA, permutations)
      for (i in 1:permutations) {
        indx <- permuted.index(length(A), strata)
        take <- A[indx]
        tmp[i] <- GOF(X, take)
      }
      pval.this <- sum(tmp > r.this)/permutations
      pval <- c(pval, pval.this)
    }
  }
  if (is.null(colnames(X))) 
    colnames(sol) <- paste("Dim", 1:ncol(sol), sep = "")
  else colnames(sol) <- colnames(X)
  names(r) <- names(P)
  if (!is.null(pval)) 
    names(pval) <- names(P)
  out <- list(centroids = sol, r = r, permutations = permutations, 
              pvals = pval)
  if (!missing(strata)) {
    out$strata <- deparse(substitute(strata))
    out$stratum.values <- strata
  }
  class(out) <- "factorfit"
  out
}
