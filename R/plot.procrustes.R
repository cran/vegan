"plot.procrustes" <-
  function (x, kind = 1, choices = c(1, 2), xlab, ylab, main, ...) 
{
  Yrot <- x$Yrot
  X <- x$X
  if (missing(main)) 
    main <- "Procrustes errors"
  if (kind == 1) {
    if (missing(xlab)) 
      xlab <- paste("Dimension", choices[1])
    if (missing(ylab)) 
      ylab <- paste("Dimension", choices[2])
    xrange <- range(Yrot[, choices[1]], X[, choices[1]])
    yrange <- range(Yrot[, choices[2]], X[, choices[2]])
    plot(xrange, yrange, xlab = xlab, ylab = ylab, main = main, 
         type = "n", asp = 1, ...)
    points(Yrot[, choices[1]], Yrot[, choices[2]])
    arrows(Yrot[, choices[1]], Yrot[, choices[2]], X[, choices[1]], 
           X[, choices[2]], col = "blue", len = 0.05)
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
  }
  invisible()
}
