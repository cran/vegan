"rad.veil" <-
    function (x, family = poisson, ...) 
{
    veilfun <- function(p, x) {
        p <- max(.Machine$double.eps, plogis(p))
        rnk <- -qnorm(p * ppoints(length(x)))
        sol <- glm(x ~ rnk, family = family(link = "log"))
        -logLik(sol)
    }
    x <- as.rad(x)
    p <- qlogis(0.8)
    fam <- family(link = "log")
    veil <- try(nlm(veilfun, p = p, x = x, hessian = TRUE, ...))
    if (inherits(veil, "try-error")) {
        aic <- rdf <- ln <- nl <- dev <- NA
        p <- rep(NA, 3)
        fit <- res <- wts <- rep(NA, length(x))
    }
    else {
        p <- max(.Machine$double.eps, plogis(veil$estimate))
        rnk <- -qnorm(p * ppoints(length(x)))
        ln <- glm(x ~ rnk, family = family(link = "log"))
        p <- c(coef(ln), p)
        fit <- fitted(ln)
        aic <- AIC(ln) + 2
        rdf <- df.residual(ln) - 1
        dev <- deviance(ln)
        res <- ln$residuals
        wts <- weights(ln)
    }
    names(p) <- c("log.mu", "log.sigma", "veil")
    out <- list(model = "Veil Log-Normal", family = fam, 
                y = x, coefficients = p, fitted.values = fit, aic = aic, 
                rank = 3, df.residual = rdf, deviance = dev, 
                residuals = res, prior.weights = wts)
    class(out) <- c("radline", "glm")
    out
}
