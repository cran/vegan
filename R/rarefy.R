"rarefy" <-
    function (x, sample, MARGIN = 1) 
{
    x <- as.matrix(x)
    if(missing(sample)) {
        sample <- min(apply(x, MARGIN, sum) - apply(x, MARGIN, max))
	info <- paste("The size of `sample' must be given --\nHint: max", sample,
                      "permissible in all cases.")
	stop(info)
    }
    rarefun <- function(x, sample) {
        x <- x[x>0]
        J <- sum(x)
        S.rare <- sum(1 - exp(lchoose(J-x, sample) - lchoose(J, sample)))
    }
    S.rare <- apply(x, MARGIN, rarefun, sample=sample)
    attr(S.rare,"Subsample") <- sample 
    S.rare
}
