"ordiParseFormula" <-
    function (formula, data) 
{
    Terms <- terms(formula, "Condition", data = data)
    flapart <- fla <- formula <- formula(Terms, width.cutoff = 500)
    specdata <- formula[[2]]
    X <- eval(specdata, data, parent.frame())
    X <- as.matrix(X)
    indPartial <- attr(Terms, "specials")$Condition
    mf <- Z <- NULL
    if (!is.null(indPartial)) {
        partterm <- attr(Terms, "variables")[[1 + indPartial]]
        Pterm <- deparse(partterm[[2]])
        P.formula <- as.formula(paste("~", Pterm))
        mf <- model.frame(P.formula, data, na.action = na.fail)
        Z <- model.matrix(P.formula, mf)
        formula <- update(formula, paste(".~.-", deparse(partterm, 
                                                         width.cutoff = 500)))
        flapart <- update(formula, paste(". ~ . +", Pterm))
    }
    formula[[2]] <- NULL
    if (formula[[2]] == "1" || formula[[2]] == "0") 
        Y <- NULL
    else {
        mf <- model.frame(formula, data, na.action = na.fail)
        Y <- model.matrix(formula, mf)
        if (any(colnames(Y) == "(Intercept)")) {
            xint <- which(colnames(Y) == "(Intercept)")
            Y <- Y[, -xint, drop = FALSE]
        }
    }
    list(X = X, Y = Y, Z = Z, terms = terms(fla, width.cutoff = 500), 
         terms.expand = terms(flapart, width.cutoff = 500), modelframe=mf)
}
