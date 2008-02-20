`permCheck` <- function(object, control = permControl())
{
    ## get number of possible permutations
    num.pos <- numPerms(object, control)
    if(num.pos < control$minperm) {
        control$nperm <- control$maxperm <- num.pos
        control$complete <- TRUE
    }
    retval <- list(n = num.pos, control = control)
    class(retval) <- "permCheck"
    retval
}
