"fitted.rda" <-
    function (object, model = c("CCA", "CA"), ...) 
{
    model <- match.arg(model)
    Xbar <- object[[model]]$Xbar
    if (model == "CCA") 
        Xbar <- qr.fitted(object$CCA$QR, Xbar)
    cent <- attr(Xbar, "scaled:center")
    scal <- attr(Xbar, "scaled:scale")
    if (!is.null(scal)) {
        Xbar <- sweep(Xbar, 2, scal, "*")
        attr(Xbar, "scaled:scale") <- NULL
    }
    Xbar <- sweep(Xbar, 2, cent, "+")
    attr(Xbar, "scaled:center") <- NULL
    Xbar
}
