"print.factorfit" <-
  function (x, ...) 
{
  if (x$permutations) 
    eps <- 1/x$permutations
  else eps <- .Machine$double.eps
  cat("Centroids:\n")
  print.coefmat(x$centroids, tst.ind = 1:ncol(x$centroids))
  cat("\nGoodness of fit:\n")
  out <- cbind(r2 = x$r, "Pr(>r)" = x$pvals)
  if (x$permutations) {
    print.coefmat(out, has.Pvalue = TRUE, eps.Pvalue = eps, 
                  ...)
    cat("P values based on", x$permutations, "permutations")
    if (!is.null(x$strata)) 
      cat(", stratified within", x$strata)
    cat(".\n")
  }
  else print.coefmat(out)
  invisible(x)
}
