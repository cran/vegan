"points.ordiplot" <-
function (x, what, ...) 
{
    x <- scores(x, what)
    points(x, ...)
    invisible()
}
