"factorfit" <-
function (X, P) 
{
   sol <- NULL
   P <- as.data.frame(P)
   for (i in 1:length(P)) {
     tmp <- apply(X, 2, tapply, P[[i]], mean)
     nam <- rownames(tmp)
     nam <- paste(names(P)[i], nam, sep="")
     rownames(tmp) <- nam 
     sol <- rbind(sol,tmp)
   }
   sol
}
