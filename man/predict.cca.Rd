\name{predict.cca}
\alias{goodness}
\alias{goodness.rda}
\alias{goodness.cca}
\alias{inertcomp}
\alias{fitted.cca}
\alias{fitted.rda}
\alias{residuals.cca}
\alias{residuals.rda}
\alias{predict.cca}
\alias{predict.rda}
\alias{vif.cca}
\alias{alias.cca}
\alias{coef.cca}
\alias{coef.rda}

\title{Prediction and Diagnostic Tools for Constrained Ordination (CCA, RDA) }
\description{
  Function \code{predict} can be used to find site and species scores
  with new data sets. Functions \code{goodness} and \code{inertcomp} can
  be used to assess the goodness of fit for individual sites or
  species. Function \code{vif.cca} and \code{alias.cca} can be used to
  analyse linear dependencies among constraints and conditions. In
  addition, there are some other diagnostic tools (see 'Details').
  
}
\usage{
\method{goodness}{cca}(object, display = c("species", "sites"), choices,
    model = c("CCA", "CA"), statistic = c("explained", "distance"),
    summarize = FALSE, ...)
inertcomp(object, display = c("species", "sites"),
    statistic = c("explained", "distance"), proportional = FALSE)
vif.cca(object)
\method{alias}{cca}(object, ...)
\method{fitted}{cca}(object, model = c("CCA", "CA"), ...)
\method{predict}{cca}(object, newdata, type = c("response", "wa", "sp", "lc"),
    rank = "full", model = c("CCA", "CA"), scaling = FALSE, ...)
\method{coef}{cca}(object, ...)
}

\arguments{
  \item{object}{A result object from \code{\link{cca}},
    \code{\link{rda}} or \code{\link{capscale}}. }
  \item{display}{Display \code{"species"} or \code{"sites"}. }
  \item{choices}{Axes shown. Default is to show all axes of the \code{"model"}. }
  \item{model}{Show constrained (\code{"CCA"}) or unconstrained
    (\code{"CA"}) results. }
  \item{statistic}{Stastic used: \code{"explained"} gives the cumulative
  percentage accounted for, \code{"distance"} shows the residual
  distances. }
  \item{summarize}{Show only the accumulated total. }
  \item{proportional}{Give the inertia components as proportional for
    the corresponding total.}
  \item{newdata}{New data frame to be used in
    prediction of species and site scores.  For \code{type = "wa"} and
    \code{type = "sp"} this must be the community data, for \code{type =
    "lc"} a data frame of environmental data (constraints and
  conditions), and with \code{type = "response"} this is ignored.}
  \item{type}{The type of prediction: \code{"response"} 
    gives an approximation of the original data matrix, \code{"wa"} the
    site scores as weighted averages of the community data, \code{"lc"}
    the site scores as linear combinations of environmental data, and
    \code{"sp"} the species scores..} 
  \item{rank}{The rank or the number of axes used in the approximation.
    The default is to use all axes (full rank) of the \code{"model"}.}
  \item{scaling}{Scaling or predicted scores
  with the same  meaning as in \code{\link{cca}}, \code{\link{rda}} and
  \code{\link{capscale}}.}
  \item{\dots}{Other parameters to the functions.}
}
\details{
  Function \code{goodness} gives the diagnostic statistics for species
  or sites. The alternative statistics are the cumulative proportion of
  inertia accounted for by the axes, or the residual distance left
  unaccounted for.  The conditional (``partialled out'') constraints are
  always regarded as explained and included in the statistics.

  Function \code{inertcomp} decomposes the inertia into partial,
  constrained and unconstrained components for each site or
  species. Instead of inertia, the function can give the total
  dispersion or distances from the centroid for each component.

  Function \code{vif.cca} gives the variance inflation factors for each
  constraint or contrast in factor constraints. In partial ordination,
  conditioning variables are analysed together with constraints. Variance
  inflation is a diagnostic tool to identify useless constraints. A
  common rule is that values over 10 indicate redundant
  constraints. If later constraints are complete linear combinations of
  conditions or previous constraints, they will be completely removed
  from the estimation, and no biplot scores or centroids are calculated
  for these aliased constraints. A note will be printed with default
  output if there are aliased constraints. Function \code{alias} will
  give the linear coefficients defining the aliased constraints. 

  Function \code{fitted} gives the approximation of the original data
  matrix from the ordination result.  Function \code{residuals} gives
  the approximation of the original data from the unconstrained
  ordination. 
  The \code{fitted.cca} and
  \code{residuals.cca} function
  both have the same marginal totals as the original data matrix, and
  their entries do not add up to the original data. They are defined so
  that for model \code{mod <- cca(y ~ x)}, \code{cca(fitted(mod))} is equal
  to constrained ordination, and \code{cca(residuals(mod))} is equal to
  unconstrained part of the ordination.

  Function \code{predict} can find the estimate of the original data
  matrix (\code{type = "response"}) with any rank. With \code{rank =
    "full"} it is identical to \code{fitted}.  In addition, the function
  can find the species scores or site scores from the community data
  matrix.  The function can be used with new data, and it can be used to
  add new species or site sccores to existing ordinations.  The function
  returns (weighted) orthornormal scores by default, and you must
  specify explicit \code{scaling} to
  add those scores to ordination diagrams. With
  \code{type = "wa"} the function finds the site scores from species
  scores. In that case, the new data can contain new sites, but species
  must match in the original and new data.  With \code{type = "sp"} the
  function finds species scores from site constraints (linear
  combination scores). In that case the new data can contain new
  species, but sites must match in the original and new
  data. With \code{type = "lc"} the function finds the linear
  combination scores for sites from environmental data. In that case the
  new data frame must contain all constraining and conditioning environmental
  variables of the model formula. If a completely new data frame is created,
  extreme care is needed defining variables similarly as in the original
  model, in particular with (ordered) factors.

  Function \code{coef} will give the regression coefficients from centred
  environmental variables (constraints and conditions) to linear
  combination scores. The coefficients are for unstandardized environmental
  variables. The coefficients will be \code{NA} for aliased effects.
}
\value{
  The functions return matrices or vectors as is appropriate.
}
\references{
  Greenacre, M. J. (1984). Theory and applications of correspondence
  analysis. Academic Press, London.

  Gross, J. (2003). Variance inflation factors. \emph{R News} 3(1),
  13--15. 
}
\author{Jari Oksanen. The \code{vif.cca} relies heavily on the code by
  W. N. Venables. \code{alias.cca} is a simplified version of
  \code{\link{alias.lm}}.}

\note{

  It is a common practise to use \code{goodness} statistics to remove
  species from ordination plots, but this may not be a good idea, as the
  total inertia is not a meaningful concept in \code{cca}, in particular
  for rare species.

  Function \code{vif} is defined as generic in package \code{car}
  (\code{\link[car]{vif}}), but if you have not loaded that package
  you must specify the call as \code{vif.cca}.  Variance inflation
  factor is useful diagnostic tool for  detecting nearly collinear
  constraints, but these are not a problem with algorithm used in this
  package to fit a constrained ordination.
}

\seealso{\code{\link{cca}}, \code{\link{rda}}, \code{\link{capscale}}, \code{\link[car]{vif}}. }
\examples{
data(dune)
data(dune.env)
mod <- cca(dune ~ A1 + Management + Condition(Moisture), data=dune.env)
goodness(mod)
goodness(mod, summ = TRUE)
# Inertia components
inertcomp(mod, prop = TRUE)
inertcomp(mod, stat="d")
# Definition of the concepts 'fitted' and 'residuals'
mod
cca(fitted(mod))
cca(residuals(mod))
# vif.cca 
vif.cca(mod)
# Aliased constraints
mod <- cca(dune ~ ., dune.env)
mod
vif.cca(mod)
alias(mod)
with(dune.env, table(Management, Manure))
# Remove rare species (freq==1) from 'cca' and find their scores
# 'passively'.
freq <- specnumber(dune, MARGIN=2)
freq
mod <- cca(dune[, freq>1] ~ A1 + Management + Condition(Moisture), dune.env)
predict(mod, type="sp", newdata=dune[, freq==1], scaling=2)
# New sites
predict(mod, type="lc", new=data.frame(A1 = 3, Management="NM", Moisture="2"), scal=2)
}
\keyword{ multivariate }

