"mantel" <-
function (xdis, ydis, method="pearson", permutations=1000) 
{
   require(mva)
   xdis <- as.dist(xdis)
   ydis <- as.vector(as.dist(ydis))
   tmp <- cor.test(as.vector(xdis), ydis, method=method)
   statistic <- as.numeric(tmp$estimate)
   variant <- tmp$method
   if (permutations) {
     N <- attributes(xdis)$Size
     perm <- rep(0, permutations)
     for (i in 1:permutations) {
       take <- sample(N, N)
       permvec <- as.vector(as.dist(as.matrix(xdis)[take,take]))
       perm[i] <- cor.test(permvec, ydis, method=method)$estimate
     }
     signif <- sum(perm >= statistic)/permutations
   } else {
     signif <- NA
     perm <- NULL 
   }
   res <- list(Call = match.call(), method=variant, statistic=statistic, 
               signif=signif, perm=perm, permutations=permutations)
   class(res) <- "mantel"
   res 
}
