"decostand" <-
  function (x, method, MARGIN) 
{
  x <- as.matrix(x)
  METHODS <- c("total", "max", "frequency", "normalize", "range", 
               "standardize", "pa")
  method <- match.arg(method, METHODS)
  switch(method, total = {
    if (missing(MARGIN)) 
      MARGIN <- 1
    tmp <- apply(x, MARGIN, sum)
    x <- sweep(x, MARGIN, tmp, "/")
  }, max = {
    if (missing(MARGIN)) 
      MARGIN <- 2
    tmp <- apply(x, MARGIN, max)
    x <- sweep(x, MARGIN, tmp, "/")
  }, frequency = {
    if (missing(MARGIN)) 
      MARGIN <- 2
    tmp <- apply(x, MARGIN, sum)
    fre <- apply(x > 0, MARGIN, sum)
    tmp <- fre/tmp
    x <- sweep(x, MARGIN, tmp, "*")
  }, normalize = {
    if (missing(MARGIN)) 
      MARGIN <- 1
    tmp <- apply(x^2, MARGIN, sum)
    tmp <- sqrt(tmp)
    x <- sweep(x, MARGIN, tmp, "/")
  }, range = {
    if (missing(MARGIN)) 
      MARGIN <- 2
    tmp <- apply(x, MARGIN, min)
    ran <- apply(x, MARGIN, max)
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
  })
  return(x)
}
