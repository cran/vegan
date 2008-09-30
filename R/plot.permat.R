## S3 plot method for permat
`plot.permat` <-
function(x, ...)
{
    n <- attr(x, "times")
    bray <- numeric(n)
    for (i in 1:n) bray[i] <- sum(abs(x$orig-x$perm[[i]]))/sum(x$orig+x$perm[[i]])
    plot(bray,type="n",ylab="Bray-Curtis dissimilarity",xlab="Runs", ...)
    lines(bray,col="red")
    lines(lowess(bray),col="blue",lty=2)
    title(sub=paste("(mean = ", substitute(z, list(z=round(mean(bray),3))), 
        ", min = ", substitute(z, list(z=round(min(bray),3))),
        ", max = ", substitute(z, list(z=round(max(bray),3))), ")", sep=""))
    invisible(NULL)
}
