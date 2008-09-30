## permatfull function
`permatfull` <-
function(m, fixedmar="both", reg=NULL, hab=NULL, mtype="count", times=100)
{
    if (!identical(all.equal(m, round(m)), TRUE))
       stop("function accepts only integers (counts)")
    mtype <- match.arg(mtype, c("prab", "count"))
    count <- mtype == "count"
    fixedmar <- match.arg(fixedmar, c("none", "rows", "columns", "both"))
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
    for (k in 1:times)
        perm[[k]] <- matrix(0, n.row, n.col)
    for (j in 1:nstr) {
    id <- which(str == j)
        if (fixedmar == "none")
            for (i in 1:times)
                if (count) perm[[i]][id,] <- matrix(sample(m[id,]), length(id), n.col)
                else perm[[i]][id,] <- commsimulator(m[id,], method="r00")
        if (fixedmar == "rows")
            for (i in 1:times)
                if (count) perm[[i]][id,] <- apply(m[id,], 2, sample)
                else perm[[i]][id,] <- commsimulator(m[id,], method="r0")
        if (fixedmar == "columns")
            for (i in 1:times)
                if (count) perm[[i]][id,] <- t(apply(m[id,], 1, sample))
                else perm[[i]][id,] <- commsimulator(m[id,], method="c0")
        if (fixedmar == "both")
            for (i in 1:times)
                if (count) perm[[i]][id,] <- r2dtable(1, apply(m[id,], 1, sum), apply(m[id,], 2, sum))[[1]]
                else perm[[i]][id,] <- commsimulator(m[id,], method="quasiswap")
        }
    specs <- list(reg=reg, hab=hab)
    out <- list(call=match.call(), orig=m, perm=perm, specs=specs)
    attr(out, "mtype") <- mtype
    attr(out, "ptype") <- "full"
    attr(out, "fixedmar") <- fixedmar
    attr(out, "times") <- times
    class(out) <- c("permat", "list")
    return(out)
}
