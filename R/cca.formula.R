"cca.formula" <-
    function (formula, data) 
{
    if (missing(data)) {
        data <- parent.frame()
    }
    d <- ordiParseFormula(formula, data)
    sol <- cca.default(d$X, d$Y, d$Z)
    if(!is.null(sol$CCA))
        sol$CCA$centroids <- centroids.cca(sol$CCA$wa.eig,
                                           d$modelframe, sol$rowsum)
    sol$terms <- d$terms
    sol$call <- match.call()
    sol$call[[1]] <- as.name("cca")
    sol$call$formula <- formula(d$terms, width.cutoff = 500)
    sol
}
