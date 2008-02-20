`anova.ccabymargin` <-
    function(object, step=100,...)
{ 
    trms <- drop.scope(object)
    alltrms <- labels(terms(object$terminfo))
    keep <- trms %in% alltrms
    trms <- trms[keep]
    cond <- alltrms %in% trms
    if (sum(!cond) > 0) {
        keepfla <- reformulate(alltrms[!cond])
        keepfla <- update(keepfla, ~ Condition(.))
        keepfla <- deparse(keepfla, width.cutoff=500)
    } else {
        keepfla <- "~"
    }
    ntrms <- length(trms)
    bigperm <- 0
    for (i in 1:ntrms) {
        fla <- paste(keepfla, trms[i], sep="+") 
        if (ntrms > 1) {
            updfla <- paste("Condition(",paste(trms[-i], collapse="+"), ")")
            fla <- paste(fla, updfla, sep="+")
        }
        tmp <- update(object, fla)
        if (is.null(tmp$CCA))
            stop("cannot analyse by = \"margin\" with complete aliasing")
        tmp <- anova(tmp, step=step, ...)
        ## Meaning is to start every permutation from the same seed, but
        ## get the seed of the longest simulation and reset the RNG
        ## to that state when exiting the function
        if (tmp[1,"N.Perm"] > bigperm) {
            bigperm <- tmp[1, "N.Perm"]
            bigseed <- get(".Random.seed", envir = .GlobalEnv,
                           inherits = FALSE)
        }
        if (i == 1) {
            seed <- attr(tmp, "Random.seed")
            sol <- tmp
        }
        else {
            sol <- rbind(sol[1:(i-1),], as.matrix(tmp[1,]), sol[i,])
            assign(".Random.seed", seed, envir = .GlobalEnv)
        }
    }
    ## Put RNG at the end of the longest simulation
    assign(".Random.seed", bigseed, envir = .GlobalEnv)
    rownames(sol)[1:ntrms] <- trms
    head <- attr(sol, "heading")
    head[1] <- paste(head[1], "Marginal effects of terms\n", sep="")
    head[2] <- paste("Model:", c(object$call))
    attr(sol, "heading") <- head
    sol
}

