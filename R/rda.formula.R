"rda.formula" <-
function (formula, data, scale=FALSE) 
{
    if (missing(data)) {
        data <- parent.frame()
    }
    d <- ordiParseFormula(formula, data)
    sol <- rda.default(d$X, d$Y, d$Z, scale)
    sol$Call <- match.call()
    sol$Call[[1]] <- as.name("rda")
    sol
}
