"identify.ordiplot" <-
function (x, what, ...) 
{
    x <- scores(x, what)
    out <- identify(x, labels = rownames(x), ...)
    out
}
