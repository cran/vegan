`taxa2dist` <-
    function (x, varstep = FALSE, check = TRUE) 
{
    rich <- apply(x, 2, function(taxa) length(unique(taxa)))
    S <- nrow(x)
    if (check) { 
        keep <- rich < S & rich > 1
        rich <- rich[keep]
        x <- x[, keep]
    }
    i <- rev(order(rich))
    x <- x[, i]
    rich <- rich[i]
    if (varstep) {
        add <- -diff(c(nrow(x), rich, 1))
        add <- add/c(S, rich)
        add <- add/sum(add) * 100
    }
    else {
        add <- rep(100/(ncol(x) + check), ncol(x) + check)
    }
    if (!is.null(names(add))) 
        names(add) <- c("Base", names(add)[-length(add)])
    if (!check)
        add <- c(0, add)
    out <- matrix(add[1], nrow(x), nrow(x))
    for (i in 1:ncol(x)) {
        out <- out + add[i + 1] * outer(x[, i], x[, i], "!=")
    }
    out <- as.dist(out)
    attr(out, "steps") <- add
    attr(out, "Labels") <- rownames(x)
    out
}
