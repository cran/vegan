"print.permutest.cca" <-
function (x, ...) 
{
  cat("\nPermutation test for CCA call:\n")
  cat(deparse(x$Call),"\n\n")
  Pval <- sum(x$F.perm >= x$F.0)/x$nperm
  cat("Test for significance of all constrained eigenvalues\n")
  cat("Pseudo-F:\t", x$F.0,"\n")
  cat("Significance:\t",format.pval(Pval, eps=1/x$nperm),"\n")
  cat("Based on", x$nperm, "permutations under",x$model, "model.\n\n")
  invisible(x)
}
