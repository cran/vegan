"surf" <-
function (x, y, choices = c(1, 2), knots = 10, family = "gaussian", 
    col = "red", thinplate = TRUE, add = FALSE, ...) 
{
    require(mgcv)
    require(akima)
    X <- scores(x, choices = choices, display = "sites")
    x1 <- X[, 1]
    x2 <- X[, 2]
    if (thinplate)
       mod <- gam(y ~ s(x1, x2, k = knots), family = family)
    else
       mod <- gam(y ~ s(x1, k=knots) + s(x2, k=knots), family = family)
    fit <- predict(mod, type = "response")
    if (!add) {
        plot(X, asp = 1, ...)
        mtext(deparse(substitute(y)))
    }
    contour(interp(x1, x2, fit, duplicate="mean"), col = col, add = TRUE)
    return(mod)
}
