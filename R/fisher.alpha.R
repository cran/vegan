"fisher.alpha" <-
    function (x, MARGIN = 1, se = FALSE, ...) 
{
    x <- as.matrix(x)
    Dev.logseries <- function(n.r, p) {
        r <- as.numeric(names(n.r))
        mu <- p[1]*p[2]^r/r
        lhood <- -sum(n.r*(log(mu/n.r)+1)) - p[1]*log(1-p[2])
        lhood
    }
    alpha.fun <- function(x) {
        p <- c(diversity(x, index="invsimpson", MARGIN=2), 0.5)
        n.r <- table(x[x>0])
        LSeries <- nlm(Dev.logseries, n.r=n.r, p = p,  hessian=TRUE, ...)
        LSeries$df.residual <- sum(x > 0) - 2
        LSeries
    }
    tmp <- apply(x, MARGIN, alpha.fun)
    out <- unlist(lapply(tmp, function(x) x$estimate[1]))
    if (se) {
        out <- list(alpha=out)
        out$se <- unlist(lapply(tmp, function(x)
                                sqrt(diag(solve(x$hessian)))[1]))
        out$df.residual <- unlist(lapply(tmp, df.residual))
        out$code <- unlist(lapply(tmp, function(x) x$code))
        out <- as.data.frame(out)
    }
    out
}
