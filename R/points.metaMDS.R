"points.metaMDS" <-
    function (x, display = c("sites", "species"), choices = c(1,2),  ...) 
{
    display <- match.arg(display)
    x <- scores(x, display, choices)
    points(x, ...)
    invisible()
}
