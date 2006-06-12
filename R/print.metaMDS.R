"print.metaMDS" <-
function (x, ...) 
{
    cat("\nCall:\n")
    cat(deparse(x$call), "\n\n")
    cat("Nonmetric Multidimensional Scaling using isoMDS (MASS package)\n\n")
    cat("Data:    ", x$data, "\n")
    cat("Distance:", x$distance, "\n\n")
    cat("Dimensions:", x$dims, "\n")
    cat("Stress:    ", x$stress, "\n")
    if (x$converged) 
        cat("Two convergent solutions found after", x$tries, 
            "tries\n")
    else cat("No convergent solutions - best solution after", 
        x$tries, "tries\n")
    z <- x$points
    scal <- c(if (attr(z, "centre")) "centring", if (attr(z, 
        "pc")) "PC rotation", if (attr(z, "halfchange")) "halfchange scaling")
    if(!length(scal))
        scal <- "as is"
    cat("Scaling:", paste(scal, collapse = ", "), "\n")
    cat("\n")
    invisible(x)
}

