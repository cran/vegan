"procrustes" <-
  function (X, Y, scale = TRUE) 
{
  if (any(attributes(X)$names == "points"))
    X <- X$points
  if (any(attributes(Y)$names == "points"))
    Y <- Y$points
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  ctrace <- function(MAT) sum(diag(crossprod(MAT)))
  c <- 1
  xmean <- apply(X, 2, mean)
  ymean <- apply(Y, 2, mean)
  X <- scale(X, scale = FALSE)
  Y <- scale(Y, scale = FALSE)
  XY <- crossprod(X, Y)
  sol <- svd(XY)
  A <- sol$v %*% t(sol$u)
  if (scale) {
    c <- sum(sol$d)/ctrace(Y)
  }
  Yrot <- c * Y %*% A
  b <- xmean - t(A %*% ymean)
  R2 <- ctrace(X) + c * c * ctrace(Y) - 2 * c * sum(sol$d)
  reslt <- list(Yrot = Yrot, X = X, ss = R2, rotation = A, 
                translation = b, scale = c, call = match.call())
  class(reslt) <- "procrustes"
  return(reslt)
}
"plot.procrustes" <-
  function (x, kind = 1, ...) 
{
  Yrot <- x$Yrot
  X <- x$X
  if (kind == 1) {
    library(MASS)
    eqscplot(X, type = "n", ...)
    points(Yrot, ...)
    arrows(Yrot[, 1], Yrot[, 2], X[, 1], X[, 2], col = "blue", 
           len = 0.05, ...)
  }
  else if (kind == 2) {
    res <- residuals(x)
    q <- quantile(res)
    plot(res, type = "h", ylab = "Procrustes error", ...)
    abline(h = q[2:4], lty = c(2, 1, 2))
  }
  invisible()
}
"print.procrustes" <-
  function (x, digits = max(3, getOption("digits") - 3), ...) 
{
  cat("\nCall:\n")
  cat(deparse(x$call), "\n\n")
  cat("Procrustes sum of squares:\n")
  cat(formatC(x$ss, digits = digits), "\n\n")
  invisible(x)
}
"print.summary.procrustes" <-
  function (x, digits = max(3, getOption("digits") - 3), ...) 
{
  cat("\nCall:\n")
  cat(deparse(x$call), "\n")
  cat("\nNumber of objects:", x$n, "   Number of dimensions:", 
      x$k, "\n")
  cat("\nProcrustes sum of squares:  ")
  cat("\n", formatC(x$ss, digits = digits), "\n")
  cat("Procrustes root mean squared error: ")
  cat("\n", formatC(x$rmse, digits = digits), "\n")
  cat("Quantiles of Procrustes errors:\n")
  nam <- c("Min", "1Q", "Median", "3Q", "Max")
  rq <- structure(quantile(x$resid), names = nam)
  print(rq, digits = digits, ...)
  cat("\n")
  invisible()
}
"residuals.procrustes" <-
  function (object) 
{
  distance <- object$X - object$Yrot
  resid <- apply(distance^2, 1, sum)
  resid <- sqrt(resid)
  resid
}
"summary.procrustes" <-
  function (object, ...) 
{
  ans <- object[c("call", "ss")]
  n <- nrow(object$Yrot)
  k <- ncol(object$Yrot)
  ans$resid <- residuals(object)
  rmse <- sqrt(object$ss/n)
  ans$n <- n
  ans$k <- k
  ans$rmse <- rmse
  class(ans) <- "summary.procrustes"
  ans
}
