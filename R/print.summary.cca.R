"print.summary.cca" <-
function (x,  digits = x$digits, ...) 
{
   cat("\nCall:\n")
   cat(deparse(x$Call), "\n")
   cat("\nPartioning of Mean Squared Contingency Coefficient:\n")
   out <- rbind("Total"=x$tot.chi, "Conditioned out"=x$partial.chi,
                          "Constrained" = x$constr.chi, "Unconstrained" = 
                           x$unconst.chi)
   colnames(out) <- ""
   print(out, digits=digits, ...)
   cat("\nEigenvalues, and their contribution to the Mean Squared Contingency Coefficient\n")
   if (!is.null(x$partial.chi)) {
     cat("after removing the contribution of conditiniong variables\n")
   }
   cat("\n")
   out <- rbind("lambda"= c(x$ev.con, x$ev.uncon), 
                 "accounted"=c(x$ev.con.account, x$ev.uncon.account))
   print(out, digits=digits, ...)
   cat("\nScaling", x$scaling, "for species and site scores\n")
   if (x$scaling == 2) {
      ev.ent <- "Species"
      other.ent <- "Sites"
   } else {
      ev.ent <- "Sites"
      other.ent <- "Species"
   }
   cat("--",ev.ent, "are scaled proportional to eigenvalues\n")
   cat("--",other.ent, "are unscaled: weighted dispersion equal")
   cat(" on all dimensions\n")
   cat("\n\nSpecies scores\n\n")
   print(x$species, digits=digits, ...)
   cat("\n\nSite scores (weighted averages of species scores)\n\n")
   print(x$sites, digits=digits, ...)
   if (!is.null(x$constraints)) {
     cat("\n\nSite constraints (linear combinations of constraining variables)\n\n")
     print(x$constraints, digits=digits, ...)
   }
   if (!is.null(x$biplot)) {
     cat("\n\nBiplot scores for constraining variables\n\n")
     print(x$biplot, digits=digits, ...)
   }
   cat("\n")
   invisible(x)
}
