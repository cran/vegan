## permatswap function
`permatswap` <-
function(m, reg=NULL, hab=NULL, mtype="count", method="swap", times=100, burnin = 10000, thin = 1000)
{
    if (!identical(all.equal(m, round(m)), TRUE))
       stop("function accepts only integers (counts)")
    mtype <- match.arg(mtype, c("prab", "count"))
    count <- mtype == "count"
    if (count) {
        method <- match.arg(method, "swap")
    } else {method <- match.arg(method, c("swap", "tswap"))}

    m <- as.matrix(m)
    n.row <- nrow(m)
    n.col <- ncol(m)
    if (mtype == "prab") m <- matrix(as.numeric(m > 0), n.row, n.col)
    if (is.null(reg) && is.null(hab)) str <- as.factor(rep(1, n.row))
    if (!is.null(reg) && is.null(hab)) str <- as.factor(reg)
    if (is.null(reg) && !is.null(hab)) str <- as.factor(hab)
    if (!is.null(reg) && !is.null(hab)) str <- interaction(reg, hab, drop=TRUE)
    levels(str) <- 1:length(unique(str))
    str <- as.numeric(str)
    nstr <- length(unique(str))
    if (any(tapply(str,list(str),length) == 1))
        stop("strata should contain at least 2 observations")
    perm <- list()
    for (i in 1:times)
        perm[[i]] <- matrix(0, n.row, n.col)

    for (j in 1:nstr) {
        id <- which(str == j)
        temp <- m[id,]
        if (count)
            for (k in 1:burnin)
                temp <- .C("swapcount", m = as.double(temp),
                        as.integer(n.row), as.integer(n.col),
                        as.integer(1), PACKAGE = "vegan")$m
        else
            for (k in 1:burnin)
                temp <- commsimulator(temp, method=method)
        for (i in 1:times) {
            if (count)
                perm[[i]][id,] <- .C("swapcount",
                                  m = as.double(temp),
                                  as.integer(n.row),
                                  as.integer(n.col),
                                  as.integer(thin),
                                  PACKAGE = "vegan")$m
	    else perm[[i]][id,] <- commsimulator(temp, method=method, thin=thin)
            temp <- perm[[i]][id,]
        }
    }
    specs <- list(reg=reg, hab=hab, burnin=burnin, thin=thin)
    out <- list(call=match.call(), orig=m, perm=perm, specs=specs)
    attr(out, "mtype") <- mtype
    attr(out, "ptype") <- "swap"
    attr(out, "fixedmar") <- "both"
    attr(out, "times") <- times
    class(out) <- c("permat", "list")
    return(out)
}
