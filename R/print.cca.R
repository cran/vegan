"print.cca" <-
function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:\n")
    cat(deparse(x$Call), "\n\n")
    chi <- rbind(x$tot.chi, x$pCCA$tot.chi, x$CCA$tot.chi, x$CA$tot.chi)
    rnk <- rbind(NA, x$pCCA$rank, x$CCA$rank, x$CA$rank)
    tbl <- cbind(chi,rnk)
    tbl <- cbind(chi, rnk)
    colnames(tbl) <- c("Inertia", "Rank")
    rn <- c("Total", "Conditional", "Constrained", "Unconstrained")
    rownames(tbl) <- rn[c(TRUE, !is.null(x$pCCA), !is.null(x$CCA), !is.null(x$CA))]
    print.coefmat(tbl, digits=digits)
    if (any(class(x) == "rda"))
        inname <- "variance"
    else
        inname <- "mean square contingency coefficient"
    cat("Inertia is", inname, "\n")
    if (!is.null(x$CCA)) {
        cat("\nEigenvalues for constrained axes:\n")
        print(x$CCA$eig, digits = digits, ...)
    }
    if (!is.null(x$CA)) {
        ax.lim <- 8
        ax.trig <- 16
        cat("\nEigenvalues for unconstrained axes:\n")
        if (x$CA$rank > ax.trig) {
            print(x$CA$eig[1:ax.lim], digits = digits, ...)
            cat("(Showed only", ax.lim, "of all", x$CA$rank, 
                "unconstrained eigenvalues)\n")
        }
        else print(x$CA$eig, digits = digits, ...)
    }
    cat("\n")
    invisible(x)
}
