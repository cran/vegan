"print.cca" <-
function(x, digits = max(3, getOption("digits") - 3), ... ) 
{
  cat("\nCall:\n")
  cat(deparse(x$Call),"\n\n")
  cat("Mean squared contingency coefficient:", 
       round(x$tot.chi,digits),"\n")
  if (!is.null(x$pCCA)) {
  cat("                     Conditioned out: ")
  cat( round(x$pCCA$tot.chi, digits),"  ( rank", x$pCCA$rank, ")\n") }
  if (!is.null(x$CCA)) {
  cat("                         Constrained: ") 
  cat(round(x$CCA$tot.chi, digits), "  ( rank",  x$CCA$rank, ")\n") }
  if (!is.null(x$CA)) {
  cat("                       Unconstrained: ")
  cat( round(x$CA$tot.chi, digits), "  ( rank", x$CA$rank, ")\n") }
  if (!is.null(x$CCA)) {
    cat("\nEigenvalues for constrained axes:\n")
    print(x$CCA$eig, digits=digits, ...)
  }
  if (!is.null(x$CA)) {
    ax.lim <- 8
    ax.trig <- 16
    cat("\nEigenvalues for unconstrained axes:\n")
    if(x$CA$rank > ax.trig) {
       print(x$CA$eig[1:ax.lim], digits=digits, ...)
       cat("(Showed only",ax.lim,"of all",x$CA$rank, 
           "unconstrained eigenvalues)\n")
    }
    else
       print(x$CA$eig, digits=digits, ...)
  }
  cat("\n")
  invisible(x)
}
