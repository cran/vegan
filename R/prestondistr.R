"prestondistr" <-
    function(x, ...)
{
    fun <- function(par, x) {
        up <- dnorm(x, par[1], par[2], log = TRUE)
        dn <- pnorm(0, par[1], par[2], lower=FALSE)
        -sum(up - log(dn))
    }
    x <- x[x>0]
    logx <- log2(x)
    p <- c(mean(logx), sd(logx))
    sol <- optim(p, fun, x = logx)
    p <- sol$par
    scale <- length(x)/sqrt(2*pi)/p[2]/pnorm(0, p[1], p[2], lower=FALSE)
    p <- c(p, scale)
    names(p) <- c("mode","width","S0")
    out <- list(freq = as.preston(x), coefficients = p)
    out$method <- "maximized likelihood to logarithmic abundances"
    class(out) <- "prestonfit"
    out
}
