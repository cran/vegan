"print.mantel" <-
function (x, digits = max(3, getOption("digits") - 3), ...) 
{
    cat("\nCall:\n")
    cat(deparse(x$Call), "\n\n")
    cat("Mantel statistic based on", x$method, "\n\n")
    cat("Mantel statistic r: ")
    cat(formatC(x$statistic, digits = digits), "\n")
    nperm <- x$permutations
    if (nperm) {
      cat("      Significance:",format.pval(x$signif, eps=1/nperm))
      cat(" (based on ", nperm, " permutations)","\n\n")
      out <- quantile(x$perm, c(0.9, 0.95, 0.975, 0.99))
      cat("Empirical upper confidence limits of r:\n")
      print(out,digits=3)
    }
    cat("\n")
    invisible(x)
}
