"text.metaMDS" <-
    function (x, display = c("sites", "species"), choices = c(1,2), ...) 
{
    display <- match.arg(display)
    x <- scores(x, display, choices)
    text(x, labels=rownames(x), ...)
    invisible()
}
