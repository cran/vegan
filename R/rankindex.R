"rankindex" <-
    function (grad, veg, indices = c("euc", "man", "gow", "bra", "kul"),
              stepacross = FALSE, method = "spearman", 
              ...) 
{
    grad <- as.matrix(grad)
    veg <- as.matrix(veg)
    span <- vegdist(grad, "eucl")
    res <- numeric(length(indices))
    names(res) <- indices
    for (i in indices) {
        y <- vegdist(veg, i)
        if (stepacross) {
            is.na(y) <- no.shared(veg)
            y <- stepacross(y, trace = FALSE, toolong=-1, ...)
        }
        res[i] <- cor(span, y, method = method)
    }
    res
}
