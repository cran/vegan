"rad.preempt" <-
    function (x, family = poisson, ...) 
{
    canfun <- function(p, x, ...) {
        p <- plogis(p)
        if (p == 1) 
            p <- 1 - .Machine$double.eps
        fv <- linkinv(logJ + log(p) + log(1 - p) * rnk)
        n <- rep(1, length(fv))
        dev <- sum(dev.resids(x, fv, wt))
        aicfun(x, n, fv, wt, dev)/2
    }
    fam <- family(link = "log")
    aicfun <- fam$aic
    linkinv <- fam$linkinv
    dev.resids <- fam$dev.resids
    x <- as.rad(x)
    rnk <- seq(along = x) - 1
    wt <- rep(1, length(x))
    logJ <- log(sum(x))
    p <- qlogis(0.1)
    canon <- try(nlm(canfun, p = p, x = x, rnk = rnk, logJ = logJ, 
                     wt = wt, hessian = TRUE, ...))
    if (inherits(canon, "try-error")) {
        aic <- rdf <- deviance <- NA
        p <- rep(NA, 1)
        fit <- residuals <- prior.weights <- rep(NA, length(x))
    }
    else {
        p <- plogis(canon$estimate)
        fit <- exp(logJ + log(p) + log(1 - p) * rnk)
        res <- dev.resids(x, fit, wt)
        deviance <- sum(res)
        residuals <- x - fit
        aic <- aicfun(x, rep(1, length(x)), fit, wt, deviance) + 2
        rdf <- length(x) - 1
    }
    names(p) <- c("alpha")
    out <- list(model = "Preemption", family = fam, y = x, coefficients = p, 
                fitted.values = fit, aic = aic, rank = 1, df.residual = rdf, 
                deviance = deviance, residuals = residuals, prior.weights = wt)
    class(out) <- c("radline", "glm")
    out
}

