`numPerms` <- function(object, control = permControl())
{
    ## number of observations in data
    nobs <- getNumObs(object)
    ## are strata present?
    use.strata <- !is.null(control$strata)
    ## check that when permuting strata or constant within strata,
    ## strata have same number of samples
    if(use.strata) {
        tab.strata <- table(control$strata)
        same.n <- length(unique(tab.strata))
        if((control$type == "strata" && same.n > 1) ||
           (control$constant == TRUE && same.n > 1))
            stop("All levels of strata must have same number of samples for chosen scheme")
    }
    ## calculate number of possible permutations
    num.pos <- if(control$type == "free") {
        if(use.strata)
            prod(factorial(tab.strata))
        else
            exp(lfactorial(nobs))
    } else if(control$type %in% c("series","grid")) {
        multi <- 2
        if(control$type == "grid")
            multi <- 4
        if(use.strata) {
            if(control$mirror) {
                if(control$constant)
                    multi * unique(tab.strata)
                else
                    prod(multi * tab.strata)
            } else {
                if(control$constant)
                    unique(tab.strata)
                else
                    prod(tab.strata)
            }
        } else {
            if(control$mirror)
                multi * nobs
            else
                nobs
        }
    } else if(control$type == "strata") {
        exp(lfactorial(length(levels(control$strata))))
    } else {
        stop("Ambiguous permutation type in 'control$type'")
    }
    num.pos
}
