\name{cca}
\alias{cca}
\alias{cca.default}
\alias{cca.formula}
\alias{print.cca}
\alias{plot.cca}
\alias{summary.cca}
\alias{print.summary.cca}
\alias{scores.cca}

\title{ [Partial] [Constrained] Correspondence Analysis }
\description{
  Function \code{cca} performs correspondence analysis, or optionally
  constrained correspondence analysis (a.k.a. canonical correspondence
  analysis), or optionally partial constrained correspondence analysis.
  These are all very popular ordination techniques in community ecology.
}
\usage{
\method{cca}{formula}(formula, data)
\method{cca}{default}(X, Y, Z)
\method{summary}{cca}(object, scaling=2, axes=6, digits, ...)
\method{plot}{cca}(x, choices=c(1,2), display=c("sp","wa","bp"), scaling=2, type, ...)
\method{scores}{cca}(x, choices=c(1,2), display=c("sp","wa","bp"),scaling=2, ...)
}

\arguments{
  \item{formula}{Model formula, where the left hand side gives the
    community data matrix, right hand side gives the constraining variables,
    and conditioning variables can be given within a special function
    \code{Condition}.}
  \item{data}{Data frame containing the variables on the right hand side
    of the model formula.}
  \item{X}{ Community data matrix. }
  \item{Y}{ Constraining matrix, typically of environmental variables.
    Can be missing. }
  \item{Z}{ Conditioning matrix, the effect of which is removed
    (`partialled out') before next step. Can be missing.}
  \item{object, x}{A \code{cca} result object.}
  \item{scaling}{Scaling for species and site scores. Either species
    (\code{2}) or site (\code{1}) scores are scaled by eigenvalues, and
    the other set of scores is left unscaled. }
  \item{axes}{Number of axes in summaries.}
  \item{digits}{Number of digits in output.}
  \item{choices}{Axes shown.}
  \item{display}{Scores shown.  These must some of the alternatives
    \code{sp} for species scores, \code{wa} for site scores, \code{lc}
    for linear constraints or ``LC scores'', or \code{bp} for biplot
    arrows.}
  \item{type}{Type of plot: partial match to \code{text}
    for text labels, \code{points} for points, and \code{none} for
    setting frames only.  If omitted, \code{text} is selected for
    smaller data sets, and \code{points} for larger.}
  \item{...}{Other parameters for \code{print} or \code{plot} functions.}
}
\details{
  Since its introduction (ter Braak 1986), constrained or canonical
  correspondence analysis has been the most popular ordination method.
  Function \code{cca} implements a version which is compliant to popular
  proprietary software \code{Canoco}, although implementation is
  completely different.  Function \code{cca} is based on Legendre &
  Legendre's (1998) algorithm: 
  Chi-square transformed data matrix is subjected to weighted linear
  regression on constraining variables, and the fitted values are
  submitted to correspondence analysis performed via singular value
  decomposition (\code{\link{svd}}). 

  The function can be called either with matrix entries for community
  data and constraints, or with formula interface.  In general, the
  formula interface is preferred, because it allows a better control of
  the model (and will be developed in further releases), and allows
  factor constraints.

  In matrix interface, the
  community data matrix \code{X} must be given, but any other data
  matrix can be omitted, and the corresponding stage of analysis is
  skipped.  If matrix \code{Z} is supplied, its effects are removed from
  the community matrix, and the residual matrix is submitted to the next
  stage.  This is called `partial' correspondence analysis.  If matrix
  \code{Y} is supplied, it is used to constrain the ordination,
  resulting in constrained or canonical correspondence analysis.
  Finally, the residual is submitted to ordinary correspondence
  analysis.  If both matrices \code{Z} and \code{Y} are missing, the
  data matrix is analysed by ordinary correspondence analysis.

  Instead of separate matrices, the model can be defined using a model
  \code{\link{formula}}.  The left hand side must be the
  community data matrix (\code{X}).  The right hand side defines the
  constraining model.  Most usual features of \code{\link{formula}}
  apply: The constraints can contain ordered or unordered factors,
  interactions among variables and functions of variables.  The defined
  \code{\link{contrasts}} are honoured in \code{\link{factor}}
  variables.  The formula can include a special term \code{Condition}
  for conditioning variables (``covariables'') ``partialled out'' before
  analysis.  So the following commands are equivalent: \code{cca(X, y,
    z)}, \code{cca(X ~ y + Condition(z))}, where \code{y} and \code{z}
  refer to single variable constraints and conditions.  

  Constrained correspondence analysis is indeed a constrained method.
  This means, that CCA does not try to display all variation in the
  data, but only the part that can be explained by the used constraints.
  Consequently, the results are strongly dependent on the set of
  constraints and their transformations or interactions among the
  constraints.  The shotgun method is to use all environmental variables
  as constraints.  However, such exploratory problems are better
  analysed with
  unconstrained methods such as correspondence analysis
  (\code{\link{decorana}}, \code{\link[mva]{ca}}) or non-metric
  multidimensional scaling (\code{\link[MASS]{isoMDS}}) and
  environmental interpretation after analysis
  (\code{\link{envfit}}, \code{\link{ordisurf}}).
  CCA is a good choice if the user has
  clear and strong \emph{a priori} hypotheses on constraints and is not
  interested in the major structure in the data set.  

  CCA is able to correct a common
  curve artefact in correspondence analysis by
  forcing the configuration into linear constraints.  However, the curve
  artefact can be avoided only with a low number of constraints that do
  not have a curvilinear relation with each other.  The curve can reappear
  even with two badly chosen constraints or a single factor.  Although
  the formula
  interface makes easy to include polynomial or interaction terms, such
  terms often allow curve artefact (and are difficult to interpret), and
  should probably be avoided.

  Partial CCA (pCCA) can be used to remove the effect of some
  conditioning or ``background'' or ``random'' variables or
  ``covariables'' before CCA proper.  In fact, pCCA compares models
  \code{cca(X ~ z)} and \code{cca(X ~ y + z)} and attributes their
  difference to the effect of \code{y} cleansed of the effect of
  \code{z}.  Some people have used the method for extracting
  ``components of variance'' in CCA.  However, if the effect of
  variables together is stronger than sum of both separately, this can
  cause increase of total Chi-square after ``partialling out'' some
  variation, and give negative ``components of variance''.  In general,
  such components of ``variance'' are not to be trusted due to
  interactions between two sets of variables.
  
  The function has \code{summary} and \code{plot} methods.  The
  \code{summary} method lists all species and site scores, and results
  may be very long.  Palmer (1993) suggested using linear constraints
  (``LC scores'') in ordination diagrams, because these gave better
  results in simulations and site scores (``WA scores'') are a step from
  constrained to unconstrained analysis.  However, McCune (1997) showed
  that noisy environmental variables (and all environmental
  measurements are noisy) destroy ``LC scores'' whereas ``WA scores''
  were little affected.  Therefore the \code{plot} function uses site
  scores (``WA scores'') as the default. This is consistent with the
  usage in general statistics and other function in \R
  (\code{\link[MASS]{lda}}, \code{\link[mva]{cancor}}). 

}
\value{
  Function returns a big object of class \code{cca}.  It has as elements
  separate lists for pCCA, CCA and CA.  These lists have information on
  total Chi-square and estimated rank of the stage.  Lists \code{CCA}
  and \code{CA} contain scores for species (\code{v}) and sites
  (\code{u}).  These site scores are linear constraints in \code{CCA} and
  weighted averages in \code{CA}.  In addition, list \code{CCA} has
  item \code{wa} for site scores and \code{biplot} for endpoints of
  biplot arrows.  All these scores are unscaled (actually, their
  weighted sum of squares is one), but they have a variant scaled by
  eigenvalue (suffix \code{eig}).  A general rule is that for
  approximation of the data (biplot in graphics), one must use one
  \code{eig} set and one unscaled set.  The result object can be
  accessed with functions \code{summary} and \code{scores.cca} which
  know how to select correct combination.  The traditional 
  alternative before CCA  was to scale sites by
  eigenvalues and leave species unscaled (\code{scaling=1}),
  so that configuration of sites
  would reflect the real structure in data (longer axes for higher
  eigenvalues).  Species scores would not reflect axis lengths, and they
  would have larger variation than species scores, which was motivated
  by some species having their optima outside studied range.  Later the
  common practice was to leave sites unscaled (\code{scaling=2}),
  so that they would have a better relation with biplot arrows.
}
\references{ The original method was by ter Braak, but the current
  implementations follows Legendre and Legendre.

  Legendre, P. and Legendre, L. (1998) \emph{Numerical Ecology}. 2nd English
  ed. Elsevier.

  McCune, B. (1997) Influence of noisy environmental data on canonical
  correspondence analysis. \emph{Ecology} 78, 2617-2623.
  
  Palmer, M. W. (1993) Putting things in even better order: The
  advantages of canonical correspondence analysis.  \emph{Ecology} 74,
  2215-2230. 
  
  Ter Braak, C. J. F. (1986) Canonical Correspondence Analysis: a new
  eigenvector technique for multivariate direct gradient
  analysis. \emph{Ecology} 67, 1167-1179.
  
}
\author{
  The responsible author was Jari Oksanen, but the code borrows heavily
  from Dave Roberts <http://labdsv.nr.usu.edu/>.
}

\seealso{
  Function \code{\link{anova.cca}} provides an ANOVA like permutation
  test for the ``significance'' of constraints.
  Function \code{\link[CoCoAn]{CAIV}} provides an alternative
  implementation of CCA (it is internally quite different). Established
  statistical alternatives include canonical correlations for continuous
  variable (\code{\link[mva]{cancor}}), and (linear) discriminant
  analysis for factor constraints (\code{\link[MASS]{lda}}).
}

\examples{
data(varespec)
data(varechem)
## Common but bad way: use all variables you happen to have in your
## environmental data matrix
vare.cca <- cca(varespec, varechem)
vare.cca
plot(vare.cca)
## Formula interface and a better model
vare.cca <- cca(varespec ~ Al + P*(K + Baresoil), data=varechem)
vare.cca
plot(vare.cca)
## `Partialling out' and `negative components of variance'
cca(varespec ~ Ca, varechem)
cca(varespec ~ Ca + Condition(pH), varechem)
}
\keyword{ multivariate }

