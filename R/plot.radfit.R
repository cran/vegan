"plot.radfit" <-
    function (x, BIC = FALSE, legend = TRUE, ...) 
{
    out <- plot(x$y, ...)
    fv <- fitted(x)
    if (BIC)
        k = log(length(x$y))
    else
        k = 2
    emph <- which.min(sapply(x$models, AIC, k = k))
    lwd <- rep(1, ncol(fv))
    lwd[emph] <- 3
    matlines(fv, lty = 1, lwd = lwd, ...)
    if (legend) {
        usr <- par("usr")
        nm <- names(x$models)
        legend(usr[2] * 0.5, 10^usr[4] * 0.9, legend = nm, lty = 1, 
               lwd = lwd, col = 1:6)
    }
    invisible(out)
}
