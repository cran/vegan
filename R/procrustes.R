"procrustes" <-
  function (X, Y, scale = TRUE) 
{
  if (is.list(X))
    X <- scores(X)
  if (is.list(Y))
    Y <- scores(Y)
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  if (ncol(X) < ncol(Y)) {
    warning("X has fewer axes than Y: X adjusted to comform Y\n")
    addcols <- ncol(Y) - ncol(X)
    for (i in 1:addcols)
      X <- cbind(X,0)
  }
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
