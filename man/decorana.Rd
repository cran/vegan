\name{decorana}
\alias{decorana}
\alias{print.decorana}
\alias{summary.decorana}
\alias{print.summary.decorana}
\alias{plot.decorana}
\alias{downweight}
\alias{scores.decorana}

\title{Detrended Correspondence Analysis and Basic Reciprocal Averaging }
\description{
  Performs detrended correspondence analysis and basic reciprocal
  averaging or orthogonal correspondence analysis.
}
\usage{
decorana(veg, iweigh=0, iresc=4, ira=0, mk=26, short=0, before=NULL,
         after=NULL)
\method{plot}{decorana}(x, choices=c(1,2), origin=TRUE, display=c("both","sites","species","none"),
     cex = 0.8, cols = c(1,2), type, ...)
\method{summary}{decorana}(object, digits=3, origin=TRUE,
     display=c("both", "species","sites","none"), ...)
downweight(veg, fraction = 5)
\method{scores}{decorana}(x, display=c("sites","species"), choices =1:4, origin=TRUE, ...)
}

\arguments{
  \item{veg}{Community data matrix. }
  \item{iweigh}{Downweighting of rare species (0: no). }
  \item{iresc}{Number of rescaling cycles (0: no rescaling). }
  \item{ira}{Type of analysis (0: detrended, 1: basic reciprocal averaging). }
  \item{mk}{Number of segments in rescaling. }
  \item{short}{Shortest gradient to be rescaled. }
  \item{before}{Hill's piecewise transformation: values before transformation. }
  \item{after}{Hill's piecewise transformation: values after
  transformation -- these must correspond to values in \code{before}.}
  \item{x, object}{A \code{decorana} result object.}
  \item{choices}{Axes shown.}
  \item{origin}{Use true origin even in detrended correspondence analysis.}
  \item{display}{Display only sites, only species, both or neither.}
  \item{cex}{Plot character size.}
  \item{cols}{Colours used for sites and species.}
  \item{type}{Type of plots, partial match to \code{"text"},
    \code{"points"} or \code{"none"}.} 
  \item{digits}{Number of digits in summary output.}
  \item{fraction}{Abundance fraction where downweighting begins.}
  \item{...}{Other parameters for \code{plot} function.}
  }
}
\details{
  In late 1970s, correspondence analysis became the method of choice for
  ordination in vegetation science, since it seemed to be able to cope
  with non-linear species responses better than principal components
  analysis.  However, even correspondence analysis produced arc-shaped
  configuration of a single gradient.  Mark Hill developed
  detrended correspondence analysis to correct two assumed `faults' in
  correspondence analysis: curvature of straight gradients and packing
  of sites at the ends of the gradient.  

  The curvature is removed by replacing the orthogonalization of axes
  with detrending.  In orthogonalization the successive axes are made
  non-correlated, but detrending should remove all systematic dependence
  between axes.  Detrending is made using a five-segment smoothing
  window with weights (1,2,3,2,1) on \code{mk} segments -- which indeed
  is more robust than the suggested alternative of detrending by
  polynomials. The
  packing of sites at the ends of the gradient is undone by rescaling
  the axes after extraction.  After rescaling, the axis is supposed to be
  scaled by `SD' units, so that the average width of Gaussian species
  responses is supposed to be one over whole axis.  Other innovations
  were the piecewise linear transformation of species abundances and
  downweighting of rare species which were regarded to have an
  unduly high influence on ordination axes.  

  It seems that detrending works actually by twisting the ordination
  space, so that the results look non-curved in two-dimensional projections
  (`lolly paper effect').  As a result, the points have usually an
  easily recognized triangle or diamond shaped pattern, obviously as a
  detrending artefact.  Rescaling works differently than commonly
  presented, too.  \code{Decorana} does not use, or even evaluate, the
  widths of species responses.  Instead, it tries to equalize the
  weighted variance of species scores on axis segments (parameter
  \code{mk} has only a small effect, since \code{decorana} finds the
  segment number from the current estimate of axis length).  This 
  equalizes response widths only for the idealized species packing
  model, where all species initially have unit width responses and
  equally spaced modes.

  Function \code{summary} prints the ordination scores,
  possible prior weights used in downweighting, and the marginal totals
  after applying these weights. Function \code{plot} plots
  species and site scores.  Classical \code{decorana} scaled the axes
  so that smallest site score was 0 (and smallest species score was
  negative), but \code{summary}, \code{plot} and
  \code{scores}  use the true origin, unless \code{origin = FALSE}.

  In addition to proper eigenvalues, the function also reports `decorana
  values' in detrended analysis. These are the values that the legacy
  code of \code{decorana} returns as `eigenvalues'.
  They are estimated internally during
  iteration, and it seems that detrending interferes the estimation 
  so that these values are generally too low and have unclear
  interpretation. Moreover, `decorana values' are estimated before
  rescaling which will change the eigenvalues. The
  proper eigenvalues are estimated after extraction of the axes and
  they are always the ratio of biased weighted variances of site and
  species scores even in detrended and rescaled solutions. The
  `decorana values' are provided only for the the compatibility with
  legacy software, and they should not be used.
}
\value{
  Function returns an object of class \code{decorana}, which has
  \code{print}, \code{summary} and \code{plot} methods.
}
\references{
  Hill, M.O. and Gauch, H.G. (1980). Detrended correspondence analysis:
  an improved ordination technique. \emph{Vegetatio} 42, 47--58.

  Oksanen, J. and Minchin, P.R. (1997). Instability of ordination
  results under changes in input data order: explanations and
  remedies. \emph{Journal of Vegetation Science} 8, 447--454.
}
\author{Mark O. Hill wrote the original Fortran code, \R port was by Jari
  Oksanen.  }
\note{
  Function \code{decorana} uses the central numerical engine of the
  original Fortran code (which is in public domain), or about 1/3 of the
  original program.  I have tried to implement the original behaviour,
  although a great part of preparatory steps were written in \R
  language, and may differ somewhat from the original code. However,
  well-known bugs are corrected and strict criteria used (Oksanen &
  Minchin 1997).

  Please
  note that there really is no need for piecewise transformation or even
  downweighting within \code{decorana}, since there are more powerful
  and extensive alternatives in \R, but these options are included for
  compliance with the original software.  If different fraction of
  abundance is needed in downweighting, function \code{downweight} must
  be applied before \code{decorana}.  Function \code{downweight} 
  indeed can be applied prior to correspondence analysis, and so it can be
  used together with \code{\link{cca}}, \code{\link[CoCoAn]{CAIV}} and
  \code{\link[multiv]{ca}} as well.

  The function finds only four axes: this is not easily changed.
}


 \seealso{
   For unconstrained ordination, non-metric multidimensional scaling in
   \code{\link[MASS]{isoMDS}} may be more robust.  Constrained (or
   `canonical') correspondence analysis can be made with
   \code{\link{cca}}.  Orthogonal correspondence analysis can be
   made with \code{\link[multiv]{ca}}, or with \code{decorana} or
   \code{\link{cca}}, but the scaling of results vary.  
 }

\examples{
data(varespec)
vare.dca <- decorana(varespec)
vare.dca
summary(vare.dca)
plot(vare.dca)
### the detrending rationale:
gaussresp <- function(x,u) exp(-(x-u)^2/2)
x <- seq(0,6,length=15) ## The gradient
u <- seq(-2,8,len=23)   ## The optima
pack <- outer(x,u,gaussresp)
matplot(x, pack, type="l", main="Species packing")
library(mva)    ## prcomp
opar <- par(mfrow=c(2,2))
plot(scores(prcomp(pack)), asp=1, type="b", main="PCA")
plot(scores(decorana(pack, ira=1)), asp=1, type="b", main="CA")
plot(scores(decorana(pack)), asp=1, type="b", main="DCA")
plot(scores(cca(pack ~ x), dis="sites"), asp=1, type="b", main="CCA")
### Let's add some noise:
noisy <- (0.5 + runif(length(pack)))*pack
par(mfrow=c(2,1))
matplot(x, pack, type="l", main="Ideal model")
matplot(x, noisy, type="l", main="Noisy model")
par(mfrow=c(2,2))
plot(scores(prcomp(noisy)), type="b", main="PCA", asp=1)
plot(scores(decorana(noisy, ira=1)), type="b", main="CA", asp=1)
plot(scores(decorana(noisy)), type="b", main="DCA", asp=1)
plot(scores(cca(noisy ~ x), dis="sites"), asp=1, type="b", main="CCA")
par(opar)
  }
\keyword{ multivariate }

