"weights.rda" <-
    function(object, display="sites", ...)
{
    display <- match.arg(display, c("sites","species"))
    n <- ifelse(display == "sites",
                nrow(object$CA$Xbar), ncol(object$CA$Xbar))
    rep(1, n)
}
