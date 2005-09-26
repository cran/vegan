"text.decorana" <-
    function (x, display = c("sites", "species"), labels, choices = 1:2, 
              origin = TRUE, select, ...) 
{
    display <- match.arg(display)
    x <- scores(x, display = display, choices = choices, origin = origin, 
                ...)
    if (!missing(labels))
        rownames(x) <- labels
    if (!missing(select)) 
        x <- x[select, , drop = FALSE]
    text(x, rownames(x), ...)
    invisible()
}
