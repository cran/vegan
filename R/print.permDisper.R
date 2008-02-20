`print.permDisper` <- function(x, digits = max(getOption("digits") - 2, 3),
                             ...)
{
    ## uses code from stats:::print.anova by R Core Development Team
    cat("\n")
    writeLines(strwrap("Permutation test for homogeneity of multivariate dispersions\n"))
    ##cat("\n")
    print(x$control)
    nc <- dim(x$tab)[2]
    cn <- colnames(x$tab)
    has.P <- substr(cn[nc], 1, 3) == "Pr("
    zap.i <- 1:(if (has.P)
                nc - 1
    else nc)
    i <- which(substr(cn, 2, 7) == " value")
    i <- c(i, which(!is.na(match(cn, "F"))))
    if (length(i))
        zap.i <- zap.i[!(zap.i %in% i)]
    tst.i <- i
    if (length(i <- grep("Df$", cn)))
        zap.i <- zap.i[!(zap.i %in% i)]
    if (length(i <- grep("N.Perm$", cn)))
        zap.i <- zap.i[!(zap.i %in% i)]
    cat("Response: Distances", sep = "\n")
    printCoefmat(x$tab, digits = digits,
                 signif.stars = getOption("show.signif.stars"),
                 has.Pvalue = has.P, P.values = has.P, cs.ind = NULL,
                 zap.ind = zap.i, tst.ind = tst.i, na.print = "", ...)
    invisible(x)
}
