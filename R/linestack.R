"linestack" <-
    function(x, cex = 0.8, label = "right", hoff = 2, air=1.1, at = 0, add = FALSE,
             axis = FALSE, ...)
{
    label<- match.arg(label, c("right","left"))
    x <- drop(x)
    op <- par(xpd=TRUE)
    x <- sort(x)
    n <- length(x)
    pos <- numeric(n)
    if (!add) {
        plot(pos, x, type="n", axes=FALSE, xlab="", ylab="")
    }
    hoff <- hoff * strwidth("m")
    ht <- air*strheight(names(x), cex=cex)
    mid <- n %/% 2
    pos[mid] <- x[mid]
    for (i in (mid+1):n) 
        pos[i] <- max(x[i], pos[i-1] + ht[i]) 
    for (i in (mid-1):1) 
        pos[i] <- min(x[i], pos[i+1] - ht[i])
    segments(at, x[1], at, x[n])
    if (label == "right") {
        text(at+hoff, pos, names(x), pos=4, cex=cex, offset=0.2, ...)
        segments(at, x, at+hoff, pos)
    } else if (label == "left") {
        text(at-hoff, pos, names(x), pos=2, cex=cex, offset=0.2, ...)
        segments(at, x, at-hoff, pos)
    }
    if (axis)
        axis(if (label=="right") 2 else 4, pos = at, las=2)
    par(op)
}

