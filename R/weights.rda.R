"weights.rda" <-
    function (object, display = "sites", ...) 
{
    display <- match.arg(display, c("sites", "species", "lc", "wa"))
    n <- if(display %in% c("sites", "lc", "wa"))
        nrow(object$CA$Xbar)
    else
        ncol(object$CA$Xbar)
    rep(1, n)
}
