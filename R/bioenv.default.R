"bioenv.default" <-
    function (comm, env, method = "spearman", index = "bray", upto=ncol(env), ...) 
{
    n <- ncol(env)
    if (n > 8) {
        if (upto < n)
            cat("Studying", sum(choose(n, 1:upto)), "of ")
        cat(2^n-1, "possible subsets (this may take time...)\n")
    }
    x <- scale(env)
    best <- list()
    comdis <- vegdist(comm, method = index)
    for (i in 1:upto) {
        sets <- ripley.subs(i, 1:n)
        if (!is.matrix(sets)) 
            sets <- as.matrix(t(sets))
        est <- numeric(nrow(sets))
        for (j in 1:nrow(sets)) est[j] <- cor.test(comdis, dist(x[, 
                                                                  sets[j, ]]), method = method)$estimate
        best[[i]] <- list(best = sets[which.max(est), ], est = max(est))
    }
    out <- list(community = deparse(substitute(comm)), environemt = deparse(substitute(env)), 
                names = colnames(env), method = method, index = index, upto = upto, 
                models = best)
    class(out) <- "bioenv"
    out
}
