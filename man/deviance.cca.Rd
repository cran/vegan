\name{deviance.cca}
\alias{deviance.cca}
\alias{deviance.rda}
\alias{deviance.capscale}
\alias{extractAIC.cca}
\title{ Statistics Resembling Deviance and AIC for Constrained Ordination}
\description{
  The functions extract statistics that resemble deviance and AIC from the
  result of constrained correspondence analysis \code{\link{cca}} or
  redundancy analysis \code{\link{rda}}.  These functions are rarely
  needed directly, but they are called by \code{\link{step}} in
  automatic model building.  Actually, \code{\link{cca}} and
  \code{\link{rda}} do not have \code{\link{AIC}} and these functions
  are certainly wrong.
}
\usage{
\method{deviance}{cca}(object, ...)
\method{extractAIC}{cca}(fit, scale = 0, k = 2, ...)
}

\arguments{
  \item{object}{the result of a constrained ordination
    (\code{\link{cca}} or \code{\link{rda}}). }
  \item{fit}{fitted model from constrained ordination.}
  \item{scale}{optional numeric specifying the scale parameter of the model,
    see \code{scale} in \code{\link{step}}.}
  \item{k}{numeric specifying the "weight" of the \emph{equivalent degrees of
    freedom} (=:\code{edf}) part in the AIC formula.}
  \item{\dots}{further arguments. }
}
\details{
  The functions find statistics that
  resemble \code{\link{deviance}} and \code{\link{AIC}} in constrained
  ordination.  Actually,
  constrained ordination methods do not have log-Likelihood, which means
  that they cannot have AIC and deviance.  Therefore you should not use
  these functions, and if you use them, you should not trust them.  If
  you use these functions, it remains as your responsibility to check
  the adequacy of the result.

  The deviance of \code{\link{cca}} is equal to Chi-square of
  the residual data matrix after fitting the constraints.  The deviance of
  \code{\link{rda}} is defined as the residual sum of squares.
  The deviance function of \code{rda} is also used for
  \code{\link{capscale}}. 
  Function \code{extractAIC} mimics
  \code{extractAIC.lm} in translating deviance to AIC.

  There is little need to call these functions directly.  However, they
  are called implicitly in \code{\link{step}} function used in automatic
  selection of constraining variables.  You should check the
  resulting model with some other criteria, because the statistics used
  here are unfounded. In particular, the penalty \code{k} is not properly
  defined, and the default \code{k = 2} is not justified
  theoretically. If you have only continuous covariates, the \code{step}
  function will base the model building on magnitude of eigenvalues, and
  the value of \code{k} only influences the stopping point (but
  variable with highest eigenvalues is not necessarily the most
  significant one in permutation
  tests in \code{\link{anova.cca}}). If you also
  have multi-class factors, the value of \code{k} will have a
  capricious effect in model building.  
 
}
\value{
  The \code{deviance} functions return ``deviance'', and
  \code{extractAIC} returns effective degrees of freedom and ``AIC''. 
}
\references{
  Godínez-Domínguez, E.  & Freire, J. (2003) Information-theoretic
  approach for selection of spatial and temporal models of community
  organization. \emph{Marine Ecology Progress Series} 253, 17--24.
}
\author{ Jari  Oksanen }

\note{
  These functions are unfounded and untested and they should not be used
  directly or implicitly.  Moreover, usual caveats in using 
  \code{\link{step}} are very valid.
}

\seealso{\code{\link{cca}}, \code{\link{rda}}, \code{\link{anova.cca}},
    \code{\link{step}}, \code{\link{extractAIC}}.}
\examples{
# The deviance of correspondence analysis equals Chi-square
data(dune)
data(dune.env)
chisq.test(dune)
deviance(cca(dune))
# Backward elimination from a complete model "dune ~ ."
ord <- cca(dune ~ ., dune.env)
ord
step(ord)
# Stepwise selection (forward from an empty model "dune ~ 1")
step(cca(dune ~ 1, dune.env), scope = formula(ord))
# ANOVA for the added variable
anova(cca(dune ~ Moisture, dune.env))
# ANOVA for the next candidate variable that was not added
anova(cca(dune ~ Condition(Moisture) + Management, dune.env), perm.max=1000)
}
\keyword{ multivariate }
\keyword{ models }
