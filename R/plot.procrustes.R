"plot.procrustes" <-
  function (x, kind = 1, axes=c(1,2), xlab="", ylab="", main="", ...) 
{
  Yrot <- x$Yrot
  X <- x$X
  if (kind == 1) {
    library(MASS)
    if (xlab == "") {
      xlab <- paste("Dimension", axes[1])
    }
    if (ylab == "") {
      ylab <- paste("Dimension", axes[2])
    }
    if (main == "") {
      main <- "Procrustes errors"
    }
    xrange <- range(Yrot[,axes[1]], X[,axes[1]])
    yrange <- range(Yrot[,axes[2]], X[,axes[2]])
    eqscplot(xrange,yrange, xlab=xlab, ylab=ylab, main=main,
             type = "n", ...)
    points(Yrot[,axes[1]], Yrot[,axes[2]])
    arrows(Yrot[, axes[1]], Yrot[, axes[2]], X[,axes[1]], X[, axes[2]],
           col = "blue", len = 0.05)
  }
  else if (kind == 2) {
    if (xlab=="")
      xlab <- "Index"
    if (ylab=="")
      ylab <- "Procrustes residual"
    res <- residuals(x)
    q <- quantile(res)
    plot(res, type = "h", xlab=xlab, ylab=ylab, main=main, ...)
    abline(h = q[2:4], lty = c(2, 1, 2))
  }
  invisible()
}
