"plot.procrustes" <-
function (x, kind = 1, choices = c(1, 2), xlab, ylab, main, 
          ar.col="blue", len=0.05, ...) 
{
    Yrot <- x$Yrot[, choices]
    X <- x$X[, choices]
    if (missing(main)) 
        main <- "Procrustes errors"
    if (kind == 1) {
        formals(arrows) <- c(formals(arrows), alist(... = ))
        if (missing(xlab)) 
            xlab <- paste("Dimension", choices[1])
        if (missing(ylab)) 
            ylab <- paste("Dimension", choices[2])
        xrange <- range(Yrot[, 1], X[, 1])
        yrange <- range(Yrot[, 2], X[,2])
        plot(xrange, yrange, xlab = xlab, ylab = ylab, main = main, 
            type = "n", asp = 1, ...)
        points(Yrot, ...)
        ow <- options(warn = -1)
        arrows(Yrot[,1], Yrot[,2], X[,1], X[,2], col = ar.col, len = len, ...)
        options(ow)
        out <- list(heads=X, points=Yrot)
        class(out) <- "ordiplot"
    }
    else if (kind == 2) {
        if (missing(xlab)) 
            xlab <- "Index"
        if (missing(ylab)) 
            ylab <- "Procrustes residual"
        res <- residuals(x)
        q <- quantile(res)
        plot(res, type = "h", xlab = xlab, ylab = ylab, main = main, 
            ...)
        abline(h = q[2:4], lty = c(2, 1, 2))
        out <- list(sites =cbind(seq(along=res), res))
        class(out) <- "ordiplot"
    }
    invisible(out)
}
