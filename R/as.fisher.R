"as.fisher" <-
    function(x, ...)
{
    if (inherits(x, "fisher"))
        return(x)
    freq <- x[x>0]
    freq <- table(freq, deparse.level=0)
    class(freq) <- "fisher"
    freq
}
