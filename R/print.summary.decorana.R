"print.summary.decorana" <-
    function (x, ...) 
{
    digits <- x$digits
    if (!is.null(x$spec.scores)) {
        cat("Species scores:\n\n")
        TABLE <- cbind(x$spec.scores, Weights = x$spec.priorweights, 
                       Totals = x$spec.totals)
        printCoefmat(TABLE, digits = digits, na.print = "", ...)
        cat("\n")
    }
    if (!is.null(x$site.scores)) {
        cat("Site scores:\n\n")
        TABLE <- cbind(x$site.scores, Totals = x$site.totals)
        printCoefmat(TABLE, digits = digits, na.print = "", ...)
        cat("\n")
    }
    invisible(x)
}
