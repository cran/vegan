"spider.cca" <-
function (x, ...) 
{
   plot.cca(x, display=c("wa","bp"), ...)
   lc <- scores(x, display="lc", ...)
   wa <- scores(x, display="wa", ...)
   segments(lc[,1], lc[,2], wa[,1], wa[,2], col="red")
   invisible()
}
