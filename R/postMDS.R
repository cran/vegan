"postMDS" <-
  function (x, dist, pc = TRUE, center = TRUE, halfchange = TRUE, 
            threshold = 0.8, nthreshold = 10) 
{
  if (any(attributes(x)$names == "points"))
    x <- x$points
  x <- as.matrix(x)
  if (center)
    x <- scale(x, scale=FALSE)
  if (pc) {
    require(mva)
    dn <- dimnames(x)
    x <- prcomp(x, center=center)$x
    dimnames(x) <- dn
  }
  if (halfchange) {
    dist <- as.vector(dist)
    ordi <- as.vector(vegdist(x, "euclidean"))
    take <- dist < threshold
    if (sum(take) < nthreshold)
      stop("Too few data points for half-change scaling")
    k <- coef(lm(dist[take] ~ ordi[take]))
    names(k) <- NULL
    hc <- (1 - k[1])/2/k[2]
    x <- x/hc
  }
  x
}
