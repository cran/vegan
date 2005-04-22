"decostand" <-
    function (x, method, MARGIN, na.rm = FALSE) 
{
    x <- as.matrix(x)
    METHODS <- c("total", "max", "frequency", "normalize", "range", 
                 "standardize", "pa", "chi.square")
    method <- match.arg(method, METHODS)
    switch(method, total = {
        if (missing(MARGIN)) 
            MARGIN <- 1
        tmp <- apply(x, MARGIN, sum, na.rm = na.rm)
        x <- sweep(x, MARGIN, tmp, "/")
    }, max = {
        if (missing(MARGIN)) 
            MARGIN <- 2
        tmp <- apply(x, MARGIN, max, na.rm = na.rm)
        x <- sweep(x, MARGIN, tmp, "/")
    }, frequency = {
        if (missing(MARGIN)) 
            MARGIN <- 2
        tmp <- apply(x, MARGIN, sum, na.rm = na.rm)
        fre <- apply(x > 0, MARGIN, sum, na.rm = na.rm)
        tmp <- fre/tmp
        x <- sweep(x, MARGIN, tmp, "*")
    }, normalize = {
        if (missing(MARGIN)) 
            MARGIN <- 1
        tmp <- apply(x^2, MARGIN, sum, na.rm = na.rm)
        tmp <- sqrt(tmp)
        x <- sweep(x, MARGIN, tmp, "/")
    }, range = {
        if (missing(MARGIN)) 
            MARGIN <- 2
        tmp <- apply(x, MARGIN, min, na.rm = na.rm)
        ran <- apply(x, MARGIN, max, na.rm = na.rm)
        ran <- ran - tmp
        x <- sweep(x, MARGIN, tmp, "-")
        x <- sweep(x, MARGIN, ran, "/")
    }, standardize = {
        if (!missing(MARGIN) && MARGIN == 1) 
            x <- t(scale(t(x)))
        else x <- scale(x)
    }, pa = {
        tmp <- dim(x)
        nam <- dimnames(x)
        x <- as.numeric(x > 0)
        dim(x) <- tmp
        dimnames(x) <- nam
    }, chi.square = {
        if (!missing(MARGIN) && MARGIN == 2) 
            x <- t(x)
        x <- sqrt(sum(x, na.rm = na.rm)) * x/outer(rowSums(x, na.rm = na.rm),
                         sqrt(colSums(x, na.rm = na.rm)))
    })
    x <- as.data.frame(x)
    x
}
