`print.oecosimu` <-
    function(x, ...)
{
    cat("oecosimu with", length(x$oecosimu$simulated), "simulations\n")
    cat("simulation method", x$oecosimu$method)
    if (length(att <- attributes(x$oecosimu$simulated)) > 0) {
        cat(" with", paste(names(att), att, collapse=", "))
    }
    cat("\n\n")
    cat("summary of simulations:\n")
    sum <- sort(c(x$oecosimu$statistic, summary(x$oecosimu$simulated)))
    print(sum, ...)
    cat("\n")
    cat("P value (2-sided): ", format.pval(x$oecosimu$pval, ...), "\n")
    cat("z (stat/sd(stat)): ", format(x$oecosimu$z), "\n\n")
    cl <- class(x)
    if (length(cl) > 1 && cl[2] != "list")
        NextMethod("print", x)
    invisible(x)   
}
