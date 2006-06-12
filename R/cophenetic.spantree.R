"cophenetic.spantree" <-
    function(x)
{
    n <- length(x$kid) + 1
    mat <- matrix(NA, nrow=n, ncol=n)
    ind <- apply(cbind(2:n, x$kid), 1, sort)
    ind <- t(ind[2:1,])
    mat[ind] <- x$dist
    d <- as.dist(mat)
    stepacross(d, path = "extended", toolong=0, trace=FALSE)
}

