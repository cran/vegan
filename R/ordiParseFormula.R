"ordiParseFormula" <-
    function (formula, data) 
{
    if (formula[[3]] == ".") {
        point <- paste(names(data), collapse = "+")
        formula <- update(formula, paste(". ~", point))
    }
    Terms <- terms(formula, "Condition", data = data)
    specdata <- formula[[2]]
    X <- eval(specdata, data, parent.frame())
    X <- as.matrix(X)
    indPartial <- attr(Terms, "specials")$Condition
    Z <- NULL
    if (!is.null(indPartial)) {
        partterm <- attr(Terms, "variables")[[1 + indPartial]]
        Pterm <- deparse(partterm[[2]])
        P.formula <- as.formula(paste("~", Pterm))
        mf <- model.frame(P.formula, data, na.action = na.fail)
        Z <- model.matrix(P.formula, mf)
        formula <- update(formula, paste(".~.-", deparse(partterm, 
                                                         width.cutoff = 500)))
    }
    formula[[2]] <- NULL
    if (formula[[2]] == "1" || formula[[2]] == "0") 
        Y <- NULL
    else {
        mf <- model.frame(formula, data, na.action = na.fail)
        Y <- model.matrix(formula, mf)
        if (any(colnames(Y) == "(Intercept)")) {
            xint <- which(colnames(Y) == "(Intercept)")
            Y <- Y[, -xint, drop=FALSE]
        }
    }
    list(X = X, Y = Y, Z = Z)
}
