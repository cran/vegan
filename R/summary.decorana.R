"summary.decorana" <-
    function (object, digits = 3, origin=TRUE,
              display=c("both", "species","sites","none"), ...) 
{
    display <- match.arg(display)
    print(object)
    if (origin) {
        object$cproj <- sweep(object$cproj, 2, object$origin, "-")
        object$rproj <- sweep(object$rproj, 2, object$origin, "-")
    }
    if (display == "both" || display == "species") {
        cat("Species scores:\n\n")
        TABLE <- cbind(round(object$cproj, digits), Weights = object$v,
                       Totals = object$adotj)
        print(TABLE, digits = digits)
        cat("\n\n")
    }
    if (display == "both" || display == "sites") {
        cat("Site scores:\n\n")
        TABLE <- cbind(round(object$rproj, digits), Totals = object$aidot)
        print(TABLE, digits = digits)
        cat("\n")
    }
    invisible()
}
