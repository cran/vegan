"plot.envfit" <-
function (x, choices = c(1,2), arrow.mul = 1, col = "blue", add = TRUE, ...) 
{
  if (!is.null(x$vectors)) {
    nvec <- ncol(x$vectors) - 1
    vect <- arrow.mul * x$vectors[,(nvec+1)] * x$vectors[,1:nvec]
  }
  if (!add) {
    xlim <- range(x$vectors[,choices[2]], x$factors[,choices[2]])
    ylim <- range(x$vectors[,choices[1]], x$factors[,choices[1]])
    if (!is.null(x$vectors))
      plot(x$vectors[,choices], xlim=xlim, ylim=ylim, asp=1, type="n", ...)
    else if (!is.null(x$factors))
      plot(x$factors[,choices], asp=1, xlim=xlim, ylim=ylim, type="n", ...)
    else
      stop("Nothing to plot")
  }
  if (!is.null(x$vectors)) {
    arrows(0, 0, vect[,1], vect[,2], len=0.05, col=col)
    text(1.1*vect, rownames(x$vectors), col=col, ...)
  }
  if (!is.null(x$factors)) {
    text(x$factors, rownames(x$factors), col=col, ...)
  }
  invisible()
}
