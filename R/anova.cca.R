"anova.cca" <-
    function (object, alpha = 0.05, beta = 0.01, step = 100, perm.max = 10000, 
              by = NULL, ...) 
{
    ## 'by' forks to the analysis by axis or by terms
    if (!is.null(by)) {
        by <- match.arg(by, c("axis", "terms"))
        if (by == "axis")
            sol <- anova.ccabyaxis(object, alpha = alpha, beta = beta, 
                               step = step, perm.max = perm.max, by = NULL, ...)
        else {
            ## No. permuations is 'step' unless '...' contains 'permutations'
            mf <- match.call(expand.dots=FALSE)
            if(!is.null(mf$...) && any(k <- pmatch(names(mf$...), "permutations",
                                                   nomatch=FALSE)))
                step <- unlist(mf$...[k==1])
            sol <- anova.ccabyterm(object, step=step, ...)
        }
        return(sol)
    }
    seed <- NULL
    betaq <- c(beta/2, 1 - beta/2)
    nperm <- 0
    unsure <- TRUE
    hits <- 0
    while (unsure && nperm < perm.max) {
        tst <- permutest.cca(object, step, ...)
        if (is.null(seed)) 
            seed <- tst$Random.seed
        nperm <- nperm + step
        hits <- hits + sum(tst$F.perm >= tst$F.0)
        fork <- qbinom(betaq, nperm, alpha)
        if (hits < fork[1] || hits > fork[2]) 
            unsure <- FALSE
    }
    Fval <- c(tst$F.0, NA)
    Pval <- c(hits/nperm, NA)
    nperm <- c(nperm, NA)
    table <- data.frame(tst$df, tst$chi, Fval, nperm, Pval)
    is.rda <- inherits(object, "rda")
    colnames(table) <- c("Df", ifelse(is.rda, "Var", "Chisq"), 
                         "F", "N.Perm", "Pr(>F)")
    head <- paste("Permutation test for", tst$method, "under", 
                  tst$model, "model\n")
    if (!is.null(tst$strata)) 
        head <- paste(head, "Permutations stratified within `", 
                      tst$strata, "'\n", sep = "")
    mod <- paste("Model:", c(object$call))
    structure(table, heading = c(head, mod), Random.seed = seed, 
              class = c("anova.cca", "anova", "data.frame"))
}

