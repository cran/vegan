"beals" <-
    function(x)
{
    x <- as.matrix(x)
    x <- ifelse(x > 0, 1, 0)
    S <- rowSums(x)
    M <- crossprod(x)
    M <- sweep(M, 2, diag(M), "/")
    for (i in 1:nrow(x)) {
        x[i,] <- rowSums(sweep(M, 2, x[i,], "*"))
    }
    x <- sweep(x, 1, S, "/")
    x
}

