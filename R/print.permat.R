## S3 print method for permat
`print.permat` <-
function(x, digits=3, ...)
{
    if (attr(x, "ptype") != "sar" & !is.null(x$specs$reg) | !is.null(x$specs$hab))
        restr <- TRUE else restr <- FALSE
    cat("Object of class 'permat'\n\nCall: ")
    print(x$call)
    cat("Matrix type:", attr(x, "mtype"), "\nPermutation type:", attr(x, "ptype"))
    cat("\nRestricted:", restr, "\nFixed margins:", attr(x, "fixedmar"))
    cat("\n\nMatrix dimensions:", nrow(x$orig), "rows,", ncol(x$orig), "columns")
    cat("\nSum of original matrix:", sum(x$orig))
    cat("\nFill of original matrix:", round(sum(x$orig>0)/(nrow(x$orig)*ncol(x$orig)),digits))
    cat("\nNumber of permuted matrices:", attr(x, "times"),"\n")
}
