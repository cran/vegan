"print.anova.cca" <-
function (x, ...) 
{
    eps <- 1/x$N.Perm[1]
    print.anova(x, eps.Pvalue = eps, ...)
    invisible(x)
}
