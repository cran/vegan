"print.anosim" <-
function (x, digits = max(3, getOption("digits") - 3), ...) 
{
    cat("\nCall:\n")
    cat(deparse(x$Call), "\n\n")
    cat("ANOSIM statistic R: ")
    cat(formatC(x$statistic, digits = digits), "\n")
    nperm <- x$permutations
    if (nperm) {
      cat("      Significance:",format.pval(x$signif, eps=1/nperm))
      cat(" (based on ", nperm, " permutations)","\n")
    }
    cat("\n")
    invisible(x)
}
