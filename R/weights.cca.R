"weights.cca" <-
    function(object, display="sites", ...)
{
    display <- match.arg(display, c("sites","species"))
    if (display == "sites") object$rowsum
    else object$colsum
}
