"cca.formula" <-
function (formula, data) 
{
    if (missing(data)) {
        data <- parent.frame()
    }
    Terms <- terms(formula, "Condition", data = data)
    specdata <- formula[[2]]
    X <- eval(specdata, data, parent.frame())
    indPartial <- attr(Terms, "specials")$Condition
    Z <- NULL
    if (!is.null(indPartial)) {
        partterm <- attr(Terms, "variables")[[1 + indPartial]]
        Pterm <- deparse(partterm[[2]])
        P.formula <- as.formula(paste("~", Pterm))
        mf <- model.frame(P.formula, data)
        Z <- model.matrix(P.formula, data)
        formula <- update(formula, 
                          paste(".~.-", deparse(partterm, width.cutoff=500)))
    }
    formula[[2]] <- NULL
    mf <- model.frame(formula, data)
    Y <- model.matrix(formula, mf)
    if (any(colnames(Y) == "(Intercept)")) {
        xint <- which(colnames(Y) == "(Intercept)")
        Y <- Y[, -xint]
    }
    if (is.null(Z)) 
        sol <- cca.default(X, Y)
    else sol <- cca.default(X, Y, Z)
    sol$Call <- match.call()
    sol$Call[[1]] <- as.name("cca")
    sol
}
