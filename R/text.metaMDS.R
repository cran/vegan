"text.metaMDS" <-
    function (x, display = c("sites", "species"),
              choices = c(1, 2), shrink = FALSE,  ...) 
{
    display <- match.arg(display)
    x <- scores(x, display = display, choices = choices, shrink = shrink)
    text(x, labels = rownames(x), ...)
    invisible()
}
