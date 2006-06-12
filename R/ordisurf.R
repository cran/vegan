"ordisurf" <-
    function (x, y, choices = c(1, 2), knots = 10, family = "gaussian", 
              col = "red", thinplate = TRUE, add = FALSE, display = "sites", 
              w = weights(x), main, nlevels = 10, levels,  ...) 
{
    w <- eval(w)
    if (!is.null(w) && length(w) == 1) 
        w <- NULL
    if (!require(mgcv)) 
        stop("Requires package `mgcv'")
    if (!require(akima)) 
        stop("Requires package `akima'")
    X <- scores(x, choices = choices, display = display, ...)
    x1 <- X[, 1]
    x2 <- X[, 2]
    if (thinplate) 
        mod <- gam(y ~ s(x1, x2, k = knots), family = family, 
                   weights = w)
    else mod <- gam(y ~ s(x1, k = knots) + s(x2, k = knots), 
                    family = family, weights = w)
    fit <- predict(mod, type = "response")
    if (!add) {
        plot(X, asp = 1, ...)
    }
    if (!missing(main) || (missing(main) && !add)) {
        if (missing(main))
            main <- deparse(substitute(y))
        title(main=main)
    }
    if (missing(levels))
        levels <- pretty(range(fit, finite=TRUE), nlevels) 
    contour(interp(x1, x2, fit, duplicate = "mean"), col = col, 
            add = TRUE, levels=levels)
    return(mod)
}
