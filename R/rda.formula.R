"rda.formula" <-
    function (formula, data, scale = FALSE) 
{
    if (missing(data)) {
        data <- parent.frame()
    }
    d <- ordiParseFormula(formula, data)
    sol <- rda.default(d$X, d$Y, d$Z, scale)
    if (!is.null(sol$CCA))
        sol$CCA$centroids <- centroids.cca(sol$CCA$wa, d$modelframe)
    sol$terms <- d$terms
    sol$call <- match.call()
    sol$call[[1]] <- as.name("rda")
    sol$call$formula <- formula(d$terms, width.cutoff = 500)
    sol
}
