"text.ordiplot" <-
function (x, what, ...) 
{
     x <- scores(x, what)
     text(x, labels = rownames(x), ...)
     invisible()
}
