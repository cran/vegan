"fitted.cca" <-
function (object, model = c("CCA","CA"), ...) 
{
  model <- match.arg(model)
  gtot <- object$grand.total
  rc <- object$rowsum %o% object$colsum
  Xbar <- object[[model]]$Xbar
  if (model == "CCA")
    Xbar <- qr.fitted(object$CCA$QR, Xbar)
  X <- (Xbar * sqrt(rc)   + rc) * gtot 
  X
}

