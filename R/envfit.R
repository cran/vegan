"envfit" <-
function (X, P) 
{  
   vectors <- NULL
   factors <- NULL
   if (is.data.frame(P)) {
      facts <- unlist(lapply(P, is.factor))
      if (sum(facts)) {
         Pfac <- P[,facts]
         P <- P[,!facts]
         vectors <- vectorfit(X, P)
         factors <- factorfit(X, Pfac)
         sol <- list(vector=vectors, factors=factors)
      }
      else 
         vectors <- vectorfit(X, P)
   }
   else
        vectors <- vectorfit(X, P)
   sol <- list(vectors = vectors, factors = factors)
   class(sol) <- "envfit"
   sol
}
