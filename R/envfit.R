"envfit" <-
function (X, P, permutations = 0, strata) 
{
    vectors <- NULL
    factors <- NULL
    seed <- NULL
    if (is.data.frame(P)) {
        facts <- unlist(lapply(P, is.factor))
        if (sum(facts)) {
            Pfac <- P[, facts]
            P <- P[, !facts]
            if (length(P)) {
                if (permutations) {
                  runif(1)
                  seed <- .Random.seed
                }
                vectors <- vectorfit(X, P, permutations, strata)
            }
            if (!is.null(seed)) 
                .Random.seed <- seed
            factors <- factorfit(X, Pfac, permutations, strata)
            sol <- list(vector = vectors, factors = factors)
        }
        else vectors <- vectorfit(X, P, permutations, strata)
    }
    else vectors <- vectorfit(X, P, permutations, strata)
    sol <- list(vectors = vectors, factors = factors)
    class(sol) <- "envfit"
    sol
}
