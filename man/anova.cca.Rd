\name{anova.cca}
\alias{anova.cca}
\alias{permutest.cca}
\alias{print.permutest.cca}
\alias{print.anova.cca}

\title{Permutation Test for Constrained Correspondence Analysis,
  Redundancy Analysis and Constrained Analysis of Principal Coordinates }
\description{
  The function performs an ANOVA like permutation test for Constrained
  Correspondence Analysis (\code{\link{cca}}), Redundancy Analysis
  (\code{\link{rda}}) or Constrained Analysis of Principal Coordinates
  (\code{\link{capscale}}) to assess the significance of constraints.
}
\usage{
\method{anova}{cca}(object, alpha=0.05, beta=0.01, step=100, perm.max=10000, ...)
permutest.cca(x, permutations=100, model=c("reduced","full"), strata)
}

\arguments{
  \item{object,x}{A result object from \code{\link{cca}}. }
  \item{alpha}{Targeted Type I error rate. }
  \item{beta}{Accepted Type II error rate. }
  \item{step}{Number of permutations during one step. }
  \item{perm.max}{Maximum number of permutations. }
  \item{\dots}{Parameters to permutest.cca. }
  \item{permutations}{Number of permutations for assessing significance
    of constraints.}
  \item{model}{Permutation model (partial match).}
  \item{strata}{An integer vector or factor specifying the strata for
    permutation. If supplied, observations are permuted only within the
    specified strata.}
}
\details{
  Functions \code{anova.cca} and \code{permutest.cca} implement an ANOVA
  like permutation test for the joint effect of constraints in
  \code{\link{cca}}, \code{\link{rda}} or \code{\link{capscale}}.
  Functions \code{anova.cca} and \code{permutest.cca} differ in printout
  style and in interface.
  Function \code{permutest.cca} is the proper workhorse, but
  \code{anova.cca} passes all parameters to \code{permutest.cca}.

  In \code{anova.cca} the number of permutations is controlled by
  targeted ``critical'' \eqn{P} value (\code{alpha}) and accepted Type
  II or rejection error (\code{beta}).  If the results of permutations
  differ from the targeted \code{alpha} at risk level given by
  \code{beta}, the permutations are
  terminated.  If the current estimate of \eqn{P} does not
  differ significantly from \code{alpha} of the alternative hypothesis,
  the permutations are
  continued with \code{step} new permutations.  
  
  The function \code{permutest.cca} implements a permutation test for
  the ``significance'' of constraints in \code{\link{cca}},
  \code{\link{rda}} or \code{\link{capscale}}.  Residuals after
  partial CCA/RDA/CAP are permuted with choice \code{model = "reduced"},
  and residuals after CCA/RDA/CAP under choice \code{model = "full"}.
  If there is no partial CCA/RDA/CAP stage, the former simply permutes
  the data. The test statistic is ``pseudo-\eqn{F}'', which is the ratio
  of constrained and unconstrained total Inertia (Chi-squares, variances
  or something similar), each divided by their respective ranks.  In plain
  CCA/RDA/CAP under \code{reduced} model, the community data is permuted, and
  the sum of all eigenvalues
  remains constant, so that pseudo-\eqn{F} and eigenvalues would give
  equal results.  In partial CCA/RDA/CAP, the effect of conditioning variables
  (``covariables'') is removed before permutation, and these residuals
  are added to the non-permuted fitted values of partial CCA (fitted
  values of \code{X ~ Z}).  Consequently, the total Chi-square is not
  fixed, and test based on pseudo-\eqn{F} would differ from the test based on
  plain eigenvalues.
}
\value{
  Function \code{permutest.cca} returns an object of class
  \code{permutest.cca} which has its own \code{print} method.  The
  function \code{anova.cca} calls \code{permutest.cca}, fills an
  \code{\link{anova}} table and uses \code{\link{print.anova}} for printing.
}
\references{
  Legendre, P. and Legendre, L. (1998). \emph{Numerical Ecology}. 2nd English
  ed. Elsevier.
}
\author{Jari  Oksanen}
\seealso{\code{\link{cca}}, \code{\link{rda}}, \code{\link{capscale}}. }

\examples{
data(varespec)
data(varechem)
vare.cca <- cca(varespec ~ Al + P + K, varechem)
anova(vare.cca)
permutest.cca(vare.cca)
## Test for adding variable N to the previous model:
anova(cca(varespec ~ N + Condition(Al + P + K), varechem), step=40)
}
\keyword{ multivariate }
\keyword{ htest }

