"anova.cca" <-
    function (object, alpha = 0.05, beta = 0.1, step = 100, perm.max = 2000, 
              ...) 
{
    betaq <- c(beta/2, 1 - beta/2)
    nperm <- 0
    unsure <- TRUE
    hits <- 0
    while (unsure && nperm < perm.max) {
        tst <- permutest.cca(object, step, ...)
        nperm <- nperm + step
        hits <- hits + sum(tst$F.perm >= tst$F.0)
        fork <- qbinom(betaq, nperm, alpha)
        if (hits < fork[1] || hits > fork[2]) 
            unsure <- FALSE
    }
    chi <- c(object$CCA$tot.chi, object$CA$tot.chi)
    df <- c(object$CCA$rank, object$CA$rank)
    Fval <- c(tst$F.0, NA)
    Pval <- c(hits/nperm, NA)
    nperm <- c(nperm, NA)
    table <- data.frame(df, chi, Fval, nperm, Pval)
    is.rda <- inherits(object, "rda")
    dimnames(table) <- list(c("Model", "Residual"), c("Df", ifelse(is.rda, 
                                                                   "Var", "Chisq"), "F", "N.Perm", "Pr(>F)"))
    head <- paste("Permutation test for", tst$method, "under", tst$model, "model\n")
    if (!is.null(tst$strata)) 
        head <- paste(head, "Permutations stratified within `", 
                      tst$strata, "'\n", sep = "")
    mod <- paste("Model:", c(object$call))
    structure(table, heading = c(head, mod), class = c("anova.cca", 
                                             "anova", "data.frame"))
}
