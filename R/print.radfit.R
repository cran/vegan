"print.radfit" <-
    function (x, ...) 
{
    cat("\nRAD models, family", x$family$family, "\n")
    cat("No. of species ", length(x$y), ", total abundance ", 
        sum(x$y), "\n\n", sep = "")
    p <- coef(x)
    aic <- sapply(x$models, AIC)
    bic <- sapply(x$models, AIC, k = log(length(x$y)))
    dev <- sapply(x$models, deviance)
    out <- cbind(p, Deviance = dev, AIC = aic, BIC = bic)
    printCoefmat(out, zap.ind=1:3, tst.ind=4:6, na.print="",...)
    invisible(x)
}
