"print.radfit.frame" <-
    function (x, ...) 
{
    if (!exists("printCoefmat", envir = NULL))
        printCoefmat <- print.coefmat
    cat("\nDeviance for RAD models:\n\n")
    out <- sapply(x, function(x) unlist(lapply(x$models, deviance)))
    printCoefmat(out, na.print = "", ...)
    invisible(x)
}
