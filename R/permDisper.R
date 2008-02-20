`permDisper` <- function(object, control = permControl(nperm = 999))
{
    if(!inherits(object, "betadisper"))
        stop("Only for class \"betadisper\"")
    nobs <- length(object$distances)
    mod <- lm(object$distances ~ object$group)
    mod.Q <- mod$qr
    p <- mod.Q$rank
    resids <- qr.resid(mod.Q, object$distances)
    res <- numeric(length = control$nperm + 1)
    res[1] <- summary(mod)$fstatistic[1]
    for(i in seq(along = res[-1])) {
        perm.resid <- resids[permuted.index2(nobs, control = control)]
        f <- qr.fitted(mod.Q, perm.resid)
        mss <- sum((f - mean(f))^2)
        r <- qr.resid(mod.Q, perm.resid)
        rss <- sum(r^2)
        rdf <- nobs - p
        resvar <- rss / rdf
        res[i+1] <- (mss / (p - 1)) / resvar
    }
    pval <- sum(res >= res[1]) / length(res)
    mod.aov <- anova(object)
    retval <- cbind(mod.aov[, 1:4], c(control$nperm, NA), c(pval, NA))
    dimnames(retval) <- list(c("Groups", "Residuals"),
                             c("Df", "Sum Sq", "Mean Sq", "F", "N.Perm",
                               "Pr(>F)"))
    retval <- list(tab = retval, control = control)
    class(retval) <- "permDisper" ##c("permDisper", class(retval))
    retval
}
