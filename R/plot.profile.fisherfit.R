"plot.profile.fisherfit" <-
    function (x, type = "l", ...) 
{
    tmp <- attributes(x, "original.fit")
    est <- tmp$original.fit$coefficients
    se <- tmp$original$std.err
    alpha <- x$alpha[, 1]
    tau <- x$alpha[, 2]
    sp <- spline(tau, alpha)
    plot(sp$x, sp$y, type = type, xlab = "alpha", ylab = "tau", 
         ...)
    abline(-est/se, 1/se, lty=2)
    abline(v=est, lty=3)
    abline(h=0, lty=3)
    invisible()
}
