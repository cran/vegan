"print.summary.decorana" <-
    function (x, ...)
{
    digits <- x$digits
    if (!is.null(x$spec.scores)) {
        cat("Species scores:\n\n")
        TABLE <- cbind(x$spec.scores, Weights = x$spec.priorweights,
                       Totals = x$spec.totals)  
        print.coefmat(TABLE, digits=digits, ...)
        cat("\n")
    }
    if (!is.null(x$site.scores)) {
        cat("Site scores:\n\n")
        TABLE <- cbind(x$site.scores, Totals = x$site.totals)
        print.coefmat(TABLE, digits = digits, ...)
        cat("\n")
    }
    invisible(x)
}
