"as.preston" <-
    function(x, ...)
{
    if (inherits(x, "preston"))
        return(x)
    freq <- x[x>0]
    freq <- ceiling(log2(freq))
    freq <- table(freq)
    class(freq) <- "preston"
    freq
}
