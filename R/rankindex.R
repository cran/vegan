"rankindex" <-
  function (grad, veg, method = "kendall") 
{
  grad <- as.matrix(grad)
  veg <- as.matrix(veg)
  span <- vegdist(grad, "eucl")
  indices <- c("euc", "man", "gow", "can", "bra", "kul")
  res <- numeric(length(indices))
  names(res) <- indices
  for (i in indices) {
    y <- as.vector(vegdist(veg, i))
    res[i] <- cor.test(span, y, method = method)$estimate
  }
  res
}

