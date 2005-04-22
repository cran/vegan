\name{predict.cca}
\alias{fitted.cca}
\alias{fitted.rda}
\alias{residuals.cca}
\alias{residuals.rda}
\alias{predict.cca}
\alias{predict.rda}
\alias{predict.decorana}
\alias{coef.cca}
\alias{coef.rda}
\alias{calibrate.cca}

\title{Prediction Tools for [Constrained] Ordination (CCA,
  RDA, DCA, CA, PCA) }
\description{
  Function \code{predict} can be used to find site and species scores
  with new data sets.  
}
\usage{
\method{fitted}{cca}(object, model = c("CCA", "CA"), ...)
\method{predict}{cca}(object, newdata, type = c("response", "wa", "sp", "lc"),
    rank = "full", model = c("CCA", "CA"), scaling = FALSE, ...)
calibrate.cca(object, newdata, rank = "full", ...)
\method{coef}{cca}(object, ...)
\method{predict}{decorana}(object, newdata, type = c("response", "sites", "species"),
    rank = 4, ...)
}

\arguments{
  \item{object}{A result object from \code{\link{cca}},
    \code{\link{rda}}, \code{\link{capscale}} or \code{\link{decorana}}. }
  \item{model}{Show constrained (\code{"CCA"}) or unconstrained
    (\code{"CA"}) results. }
  \item{newdata}{New data frame to be used in
    prediction of species and site scores or for calibration.  Usually
    this a new community data frame, but for \code{predict.cca}
    \code{type = "lc"} it must be an environment data frame, and for
    \code{type = "response"} this is ignored.}
  \item{type}{The type of prediction: \code{"response"} 
    gives an approximation of the original data matrix, \code{"wa"} the
    site scores as weighted averages of the community data, \code{"lc"}
    the site scores as linear combinations of environmental data, and
    \code{"sp"} the species scores.  In \code{predict.decorana} the
    alternatives are scores for \code{"sites"} or \code{"species"}.} 
  \item{rank}{The rank or the number of axes used in the approximation.
    The default is to use all axes (full rank) of the \code{"model"} or
    all available four axes in \code{predict.decorana}.}
  \item{scaling}{Scaling or predicted scores
  with the same  meaning as in \code{\link{cca}}, \code{\link{rda}} and
  \code{\link{capscale}}.}
  \item{\dots}{Other parameters to the functions.}
}
\details{
  Function \code{fitted} gives the approximation of the original data
  matrix from the ordination result.  Function \code{residuals} gives
  the approximation of the original data from the unconstrained
  ordination.  The \code{fitted.cca} and \code{residuals.cca} function
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

  Function \code{calibrate.cca} finds estimates of constraints from
  community ordination or \code{"wa"} scores from \code{\link{cca}},
  \code{\link{rda}} and \code{\link{capscale}}. This is often known as
  calibration, bioindication or environmental reconstruction.
  Basically, the method is similar to projecting site scores onto biplot
  arrows, but it uses regression coefficients.  The function can be called
  with \code{newdata} so that cross-validation is possible.  The
  \code{newdata} may contain new sites, but species must match in the
  original and new data  The function
  does not work with \sQuote{partial} models with \code{Condition} term,
  and it cannot be used with \code{newdata} for \code{\link{capscale}}
  results.  The results may only be interpretable for continuous variables.
  
  Function \code{coef} will give the regression coefficients from centred
  environmental variables (constraints and conditions) to linear
  combination scores. The coefficients are for unstandardized environmental
  variables. The coefficients will be \code{NA} for aliased effects.

  Function \code{predict.decorana} is similar to \code{predict.cca}.
  However, \code{type = "species"} is not available in detrended
  correspondence analysis  (DCA), because detrending destroys the mutual
  reciprocal averaging (except for the first axis when rescaling is not
  used). Detrended CA does not attempt to approximate the original data
  matrix, so \code{type = "response"} has no meaning in detrended
  analysis (except with \code{rank = 1}).
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
\author{Jari Oksanen.}


\seealso{\code{\link{cca}}, \code{\link{rda}}, \code{\link{capscale}},
  \code{\link{decorana}}, \code{\link[car]{vif}}, \code{\link{goodness.cca}}. }
\examples{
data(dune)
data(dune.env)
mod <- cca(dune ~ A1 + Management + Condition(Moisture), data=dune.env)
# Definition of the concepts 'fitted' and 'residuals'
mod
cca(fitted(mod))
cca(residuals(mod))
# Remove rare species (freq==1) from 'cca' and find their scores
# 'passively'.
freq <- specnumber(dune, MARGIN=2)
freq
mod <- cca(dune[, freq>1] ~ A1 + Management + Condition(Moisture), dune.env)
predict(mod, type="sp", newdata=dune[, freq==1], scaling=2)
# New sites
predict(mod, type="lc", new=data.frame(A1 = 3, Management="NM", Moisture="2"), scal=2)
# Calibration and residual plot
mod <- cca(dune ~ A1 + Moisture, dune.env)
pred <- calibrate.cca(mod)
pred
with(dune.env, plot(A1, pred[,"A1"] - A1, ylab="Prediction Error"))
abline(h=0)
}
\keyword{ multivariate }

