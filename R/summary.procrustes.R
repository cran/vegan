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
