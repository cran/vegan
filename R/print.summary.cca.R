"print.summary.cca" <-
function (x, digits = x$digits, ...) 
{
    cat("\nCall:\n")
    cat(deparse(x$call), "\n")
    if (x$call[1] == "cca") {
        inertname <- "Mean Square Congingency Coefficient"
        statnam <- "averages"
    }
    else {
        inertnam <- "Variance"
        statnam = "sums"
    }
    cat("\nPartitioning of ", inertnam, ":\n", sep = "")
    out <- rbind(Total = x$tot.chi, "Conditioned out" = x$partial.chi, 
        Constrained = x$constr.chi, Unconstrained = x$unconst.chi)
    colnames(out) <- ""
    print(out, digits = digits, ...)
    cat("\nEigenvalues, and their contribution to the", inertnam, 
        "\n")
    if (!is.null(x$partial.chi)) {
        cat("after removing the contribution of conditiniong variables\n")
    }
    cat("\n")
    out <- rbind(lambda = c(x$ev.con, x$ev.uncon), accounted = c(x$ev.con.account, 
        x$ev.uncon.account))
    print(out, digits = digits, ...)
    cat("\nScaling", x$scaling, "for species and site scores\n")
    if (x$scaling == 2) {
        ev.ent <- "Species"
        other.ent <- "Sites"
    }
    else if (x$scaling == 1) {
        ev.ent <- "Sites"
        other.ent <- "Species"
    }
    else {
        ev.ent <- "Both sites and species"
        other.ent <- NULL
    }
    cat("--", ev.ent, "are scaled proportional to eigenvalues\n")
    if (!is.null(other.ent)) 
        cat("--", other.ent, "are unscaled: weighted dispersion equal")
    cat(" on all dimensions\n")
    cat("\n\nSpecies scores\n\n")
    print(x$species, digits = digits, ...)
    cat("\n\nSite scores (weighted", statnam, "of species scores)\n\n")
    print(x$sites, digits = digits, ...)
    if (!is.null(x$constraints)) {
        cat("\n\nSite constraints (linear combinations of constraining variables)\n\n")
        print(x$constraints, digits = digits, ...)
    }
    if (!is.null(x$biplot)) {
        cat("\n\nBiplot scores for constraining variables\n\n")
        print(x$biplot, digits = digits, ...)
    }
    if (!is.na(x$centroids)) {
        cat("\n\nCentroids for factor constraints\n\n")
        print(x$centroids, digits = digits, ...)
    }
    cat("\n")
    invisible(x)
}
