"rad.lognormal" <-
    function (x, family = poisson, ...) 
{
    x <- as.rad(x)
    n <- length(x)
    rnk <- -qnorm(ppoints(n))
    fam <- family(link = "log")
    ln <- try(glm(x ~ rnk, family = fam))
    if (inherits(ln, "try-error")) {
        aic <- rdf <- ln <- nl <- dev <-  NA
        p <- rep(NA, 2)
        fit <- res <- wts <- rep(NA, length(x))
    }
    else {
        p <- coef(ln)
        fit <- fitted(ln)
        aic <- AIC(ln)
        rdf <- df.residual(ln)
        dev <- deviance(ln)
        res <- ln$residuals
        wts <- weights(ln)
    }
    names(p) <- c("log.mu", "log.sigma")
    out <- list(model = "Log-Normal", family = fam, y = x, 
                coefficients = p, fitted.values = fit, aic = aic, rank = 2, 
                df.residual = rdf, deviance = dev, residuals = res, 
                prior.weights = wts)
    class(out) <- c("radline", "glm")
    out
}
