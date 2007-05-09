`designdist` <-
    function (x, method = "(A+B-2*J)/(A+B)",
              terms = c("binary", "quadratic", "minimum"), name) 
{
    terms <- match.arg(terms)
    x <- as.matrix(x)
    N <- nrow(x)
    P <- ncol(x)
    if (terms == "binary") 
        x <- ifelse(x > 0, 1, 0)
    if (terms == "binary" || terms == "quadratic") 
        x <- tcrossprod(x)
    if (terms == "minimum") {
        r <- rowSums(x)
        x <- dist(x, "manhattan")
        x <- (outer(r, r, "+") - as.matrix(x))/2
    }
    d <- diag(x)
    A <- as.dist(outer(rep(1, N), d))
    B <- as.dist(outer(d, rep(1, N)))
    J <- as.dist(x)
    dis <- eval(parse(text = method))
    attr(dis, "call") <- match.call()
    if (missing(name))
        attr(dis, "method") <- paste(method, terms)
    else
        attr(dis, "method") <- name
    dis
}
