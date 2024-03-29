`specaccum` <-
    function (comm, method = "exact", permutations = 100, conditioned=TRUE,
              gamma="jack1", w = NULL, subset, ...)
{
    METHODS <- c("collector", "random", "exact", "rarefaction", "coleman")
    method <- match.arg(method, METHODS)
    if (!is.null(w) && !(method %in% c("random", "collector")))
        stop("weights 'w' can be only used with methods 'random' and 'collector'")
    if (!missing(subset)) {
        comm <- subset(comm, subset)
        w <- subset(w, subset)
    }
    x <- comm
    x <- as.matrix(x)
    x <- x[, colSums(x) > 0, drop=FALSE]
    n <- nrow(x)
    p <- ncol(x)
    accumulator <- function(x, ind) {
        rowSums(apply(x[ind, , drop=FALSE], 2, cumsum) > 0)
    }
    specaccum <- sdaccum <- sites <- perm <- NULL
    if (n == 1 && method != "rarefaction")
        message("no actual accumulation since only one site provided")
    switch(method, collector = {
        sites <- seq_len(n)
        xout <- weights <- cumsum(w)
        specaccum <- accumulator(x, sites)
        perm <- as.matrix(specaccum)
        weights <- as.matrix(weights)
    }, random = {
        permat <- getPermuteMatrix(permutations, n)
        perm <- apply(permat, 1, accumulator, x = x)
        if (!is.null(w))
            weights <- as.matrix(apply(permat, 1, function(i) cumsum(w[i])))
        sites <- seq_len(n)
        if (is.null(w)) {
            specaccum <- apply(perm, 1, mean)
            sdaccum <- apply(perm, 1, sd)
        } else {
            sumw <- sum(w)
            xout <- seq(sumw/n, sumw, length.out = n)
            intx <- sapply(seq_len(NCOL(perm)), function(i)
                           approx(weights[,i], perm[,i], xout = xout)$y)
            specaccum <- apply(intx, 1, mean)
            sdaccum <- apply(intx, 1, sd)
        }
    }, exact = {
        freq <- colSums(x > 0)
        freq <- freq[freq > 0]
        f <- length(freq)
        ldiv <- lchoose(n, 1:n)
        result <- array(dim = c(n, f))
        for (i in 1:n) {
            result[i, ] <- ifelse(n - freq < i, 0,
                                  exp(lchoose(n - freq, i) - ldiv[i]))
        }
        sites <- 1:n
        specaccum <- rowSums(1 - result)
        if (conditioned) {
            V <- result * (1 - result)
            tmp1 <- cor(x > 0)
            ind <- lower.tri(tmp1)
            tmp1 <- tmp1[ind]
            tmp1[is.na(tmp1)] <- 0
            cv <- numeric(n)
            for (i in 1:n) {
                tmp2 <- outer(sqrt(V[i, ]), sqrt(V[i, ]))[ind]
                cv[i] <- 2 * sum(tmp1 * tmp2)
            }
            V <- rowSums(V)
            sdaccum <- sqrt(V + cv)
        }else{
            Stot <- specpool(x)[,gamma]
            sdaccum1 <- rowSums((1-result)^2)
            sdaccum2 <- specaccum^2/Stot
            sdaccum <- sqrt(sdaccum1 - sdaccum2)
        }
    }, rarefaction = {
        ## rarefaction should be done on observed counts that usually
        ## have singletons. Warn here but not on every row when
        ## calling rarefy().
        minobs <- min(x[x > 0])
        if (minobs > 1)
            warning(
                gettextf("most observed count data have counts 1, but smallest count is %d", minobs))
        freq <- colSums(x)
        freq <- freq[freq > 0]
        tot <- sum(freq)
        ind <- round(seq(tot/n, tot, length = n))
        result <- matrix(NA, nrow = 2, ncol = n)
        for (i in 1:n) {
            result[, i] <- suppressWarnings(rarefy(t(freq), ind[i], se = TRUE))
        }
        specaccum <- result[1, ]
        sdaccum <- result[2, ]
        sites <- ind/tot * n
    }, coleman = {
        freq <- colSums(x > 0)
        result <- array(dim = c(n, p))
        for (i in 1:n) {
            result[i, ] <- (1 - i/n)^freq
        }
        result <- 1 - result
        sites <- seq_len(n)
        specaccum <- rowSums(result)
        sdaccum <- sqrt(rowSums(result * (1 - result)))
    })
    out <- list(call = match.call(), method = method, sites = sites,
                richness = specaccum, sd = sdaccum, perm = perm)
    if (!is.null(w)) {
        out$weights <- weights
        out$effort <- xout
    }
    if (method == "rarefaction")
        out$individuals <- ind
    ## return 'freq' for methods that are solely defined by them
    if (method %in% c("exact", "rarefaction", "coleman"))
        out$freq <- freq
    if (method == "random")
        attr(out, "control") <- attr(permat, "control")
    class(out) <- "specaccum"
    out
}
