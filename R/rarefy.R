"rarefy" <-
function (x, sample, se = FALSE, MARGIN = 1) 
{
    x <- as.matrix(x)
    if (missing(sample)) {
        sample <- min(apply(x, MARGIN, sum))
        info <- paste("The size of `sample' must be given --\nHint: Smallest site maximum",
                      sample)
        stop(info)
    }
    rarefun <- function(x, sample) {
        x <- x[x > 0]
        J <- sum(x)
        ldiv <- lchoose(J, sample)
        p1 <- ifelse(J-x < sample, 0, exp(lchoose(J - x, sample) - ldiv))
        out <- sum(1-p1)
        if(se) {
            V <- sum(p1*(1-p1))
            Jxx <- J - outer(x, x, "+")
            ind <- lower.tri(Jxx)
            Jxx <- Jxx[ind]
            V <- V + 2*sum(ifelse(Jxx < sample, 0, exp(lchoose(Jxx, sample) - ldiv))
            - outer(p1,p1)[ind])
            out <- cbind(out, sqrt(V))
        }
        out
    }
    S.rare <- apply(x, MARGIN, rarefun, sample = sample)
    if (se)
        rownames(S.rare) <- c("S","se")
    attr(S.rare, "Subsample") <- sample
    S.rare
}
