"wascores" <-
  function (x, w, expand = FALSE) 
{
  x <- as.matrix(x)
  w <- as.matrix(w)
  nc <- ncol(x)
  nr <- ncol(w)
  wa <- matrix(NA, nrow = nr, ncol = nc)
  colnames(wa) <- colnames(x)
  rownames(wa) <- colnames(w)
  for (i in 1:nr) {
    wa[i, ] <- apply(x, 2, weighted.mean, w = w[, i])
  }
  if (expand) {
    wam <- apply(wa, 2, mean)
    wa <- sweep(wa, 2, wam, "-")
    xsd <- apply(x, 2, sd)
    wasd <- apply(wa, 2, sd)
    xsd <- xsd/wasd
    wa <- sweep(wa, 2, xsd, "*")
    wam <- wam * xsd
    wa <- sweep(wa, 2, wam, "+")
  }
  wa
}
