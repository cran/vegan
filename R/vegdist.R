"vegdist" <-
    function (x, method = "bray", diag = FALSE, upper = FALSE) 
{
    if (!is.na(pmatch(method, "euclidian"))) 
        method <- "euclidean"
    METHODS <- c("manhattan", "euclidean", "canberra", "bray", "kulczynski",
                 "gower", "morisita", "horn", "mountford", "jaccard")
    method <- pmatch(method, METHODS)
    if (is.na(method)) 
        stop("invalid distance method")
    if (method == -1) 
        stop("ambiguous distance method")
    if (method == 6)
        x <- decostand(x, "range", 2)
    N <- nrow(x <- as.matrix(x))
    d <- .C("veg_distance", x = as.double(x), nr = N, nc = ncol(x), 
            d = double(N * (N - 1)/2), diag = as.integer(FALSE), 
            method = as.integer(method), PACKAGE="vegan")$d
    if (method == 10)
        d <- 2*d/(1+d)
    attr(d, "Size") <- N
    attr(d, "Labels") <- dimnames(x)[[1]]
    attr(d, "Diag") <- diag
    attr(d, "Upper") <- upper
    attr(d, "method") <- METHODS[method]
    attr(d, "call") <- match.call()
    class(d) <- "dist"
    return(d)
}
