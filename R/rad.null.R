"rad.null" <-
    function(x,  family=poisson, ...)
{
    fam <- family(link="log")
    aicfun <- fam$aic
    dev.resids <- fam$dev.resids
    x <- as.rad(x)
    nsp <- length(x)
    wt <- rep(1, nsp)
    fit <- rev(cumsum(1/nsp:1)/nsp) * sum(x)
    res <- dev.resids(x, fit, wt)
    deviance <- sum(res)
    residuals <- x - fit
    aic <- aicfun(x, wt, fit, wt, deviance) 
    rdf <- nsp
    p <- NA
    names(p) <- "S"
    out <- list(model = "Brokenstick", family=fam, y = x, coefficients = p,
                fitted.values = fit, aic = aic, rank = 0, df.residual = rdf,
                deviance = deviance, residuals = residuals, prior.weights=wt)
    class(out) <- c("radline", "glm")
    out
}

