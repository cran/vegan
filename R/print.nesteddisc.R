"print.nesteddisc" <-
function(x, ...)
{
    cat("nestedness discrepancy:", x$statistic, "\n")
    invisible(x)
}

