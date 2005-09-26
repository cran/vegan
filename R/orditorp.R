"orditorp" <-
    function (x, display, labels, choices = c(1, 2), priority, 
              tcex = 0.7, pcex, tcol = par("col"), pcol, pch = par("pch"), 
              air = 1, ...) 
{
    if (missing(pcex)) 
        pcex <- tcex
    if (missing(pcol)) 
        pcol <- tcol
    x <- scores(x, display = display, choices = choices, ...)
    if (missing(labels))
    	labels <- rownames(x)
    if (missing(priority)) 
        priority <- rowSums((scale(x)^2))
    w <- strwidth(labels, cex = tcex)/2 * air
    h <- strheight(labels, cex = tcex)/2 * air
    xx <- cbind(x[, 1] - w, x[, 1] + w, x[, 2] - h, x[, 2] + 
                h)
    is.na(priority) <- w == 0   
    ord <- rev(order(priority, na.last = FALSE))
    xx <- xx[ord, ]
    x <- x[ord,]
    labels <- labels[ord]
    tt <- logical(nrow(xx))
    tt[1] <- TRUE
    for (i in 2:(nrow(xx) - sum(is.na(priority)))) {
        j <- 1:(i - 1)
        j <- j[tt[j]]
        tt[i] <- all(xx[i, 1] > xx[j, 2] | xx[j, 1] > xx[i, 2] | 
                     xx[i, 3] > xx[j, 4] | xx[j, 3] > xx[i, 4])
    }
    if (sum(!tt)) 
        points(x[!tt , , drop = FALSE], pch = pch, cex = pcex, 
               col = pcol, ...)
    text(x[tt, , drop = FALSE], labels[tt], cex = tcex, 
         col = tcol, ...)
    names(tt) <- labels
    tt <- tt[order(ord)]
    invisible(tt)
}

