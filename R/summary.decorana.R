"summary.decorana" <-
  function (x, digits = 3, origin=TRUE,
            display=c("both", "species","sites","none"), ...) 
{
  display <- match.arg(display)
  print(x)
  if (origin) {
    x$cproj <- sweep(x$cproj, 2, x$origin, "-")
    x$rproj <- sweep(x$rproj, 2, x$origin, "-")
  }
  if (display == "both" || display == "species") {
    cat("Species scores:\n\n")
    TABLE <- cbind(round(x$cproj, digits), Weights = x$v, Totals = x$adotj)
    print(TABLE, digits = digits)
    cat("\n\n")
  }
  if (display == "both" || display == "sites") {
    cat("Site scores:\n\n")
    TABLE <- cbind(round(x$rproj, digits), Totals = x$aidot)
    print(TABLE, digits = digits)
    cat("\n")
  }
  invisible()
}
