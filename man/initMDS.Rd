\name{initMDS}
\alias{initMDS}
\alias{postMDS}
\title{ Random Start and Axis Scaling for isoMDS }
\description{
  Function \code{initMDS} gives a random  start for multidimensional
  scaling, and \code{postMDS} rforms some post-standardizations for
  multidimensional scaling, in order to make the configurations easier
  to interpret 
}
\usage{
initMDS(x, k=2)
postMDS(X, dist, pc=TRUE, center=TRUE, halfchange=TRUE, threshold=0.8,
        nthreshold=10, plot=FALSE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{Dissimilarity matrix for isoMDS.}
  \item{k}{Number of dimensions.}
  \item{X}{Configuration from multidimensional scaling. }
  \item{dist}{Dissimilarity matrix used in multidimensional scaling. }
  \item{pc}{Rotate to principal components. }
  \item{center}{Centre the configuration. }
  \item{halfchange}{Scale axes to half-change units.}
  \item{threshold}{Largest dissimilarity used in half-change scaling. }
  \item{nthreshold}{ Minimum number of points in half-change scaling. }
  \item{plot}{Show half-change scaling plot.} 
}
\details{
  Non-metric Multidimensional Scaling (NMDS) is commonly regarded as the
  most robust unconstrained ordination method in community ecology (Micnhin
  1987).  Functions \code{initMDS} and \code{postMDS} together with some
  other functions are intended to 
  help run NMDS in \code{\link[MASS]{isoMDS}} like recommended by
  Minchin (1987)  -- NMDS is not a
  strong choice unless used correctly:
  \enumerate{
    \item You should use a dissimilarity index that has a good rank
    order relation with ecological gradients.  Function
    \code{\link{rankindex}} may help in choosing an adequate index.
    Often a good \emph{a priori} choice is to use Bray--Curtis index,
    perhaps with \code{\link{wisconsin}} double standardization.  Some
    recommended indices are available in function
    \code{\link{vegdist}}.

    \item NMDS should be run with several random starts.  It is
    dangerous to follow the common default of starting with metric
    scaling (\code{\link[mva]{cmdscale}}), because this may be close to
    a local optimum which will trap the iteration.  Function
    \code{initMDS} provides such random starts.

    \item NMDS solutions with minimum stress should be compared for
    consistency.  You should be satisfied only when several minimum
    stress solutions have similar configurations as well.  In large data
    sets in particular, single points may be unstable even with about
    equal stress.  Function \code{postMDS} provides a simple solution
    for fixing the scaling of NMDS.  Function \code{\link{procrustes}}
    provides Procrustes rotation for a more formal test.
    }

  Function \code{postMDS} provides the following way of fixing the
  indeterminacy of scaling and orientation of axes in NMDS:
  Centring moves the origin to the
  average of each axis.  Principal components rotate the configuration
  so that the variance of points is maximized on first
  dimensions. Half-change scaling scales the configuration so that one 
  unit means halving of community similarity from replicate similarity.
  Half-change scaling is
  based on closer dissimilarities where the relation between ordination
  distance and community dissimilarity is rather linear; the limit is
  controlled by parameter \code{threshold}.  If there are enough points
  below this threshold (controlled by the the parameter
  \code{nthreshold}), dissimilarities are regressed on distances.
  The intercept of this regression is taken as the replicate
  dissimilarity, and half-change is the distance where similarity
  halves according to linear regression.  Obviously the method is
  applicable only for dissimilarity indices scaled to \eqn{0 \ldots 1},
  such as Kulczynski, Bray-Curtis and Canberra indices. 
}

\value{
  Function \code{initMDS} returns a random configuration which is
  intended to be used within \code{\link[MASS]{isoMDS}} only.  Function
  \code{postMDS} returns the result of \code{\link[MASS]{isoMDS}} with
  updated configuration. 
}  
\references{ Minchin, P.R. (1987)  An evaluation of relative robustness
  of techniques for ecological ordinations. \emph{Vegetatio} 71, 145-156. }

\author{ Jari Oksanen }

\seealso{\code{\link[MASS]{isoMDS}}, \code{\link[mva]{cmdscale}},
  \code{\link{procrustes}}.  }

\examples{
## The recommended way of running NMDS (Minchin 1987)
##
data(varespec)
data(varechem)
library(MASS) ## isoMDS
library(mva)  ## cmdscale: default start to isoMDS
# Select a good dissimilarity index
rankindex(scale(varechem),varespec)
rankindex(scale(varechem),wisconsin(varespec))
vare.dist <- vegdist(wisconsin(varespec), "bray")
# Get the baseline solution: start with cmdscale
mds.null <- isoMDS(vare.dist)
## See if you can get any better.
repeat{
  mds.1<- isoMDS(vare.dist, initMDS(vare.dist), maxit=200, trace=FALSE)
  if(mds.1$stress < mds.null$stress) break
}
# Scale solutions ("fix translation, rotation and scale")
mds.null <- postMDS(mds.null, vare.dist)
mds.1 <- postMDS(mds.1, vare.dist)
# Compare solutions
plot(procrustes(mds.1, mds.null))
}
\keyword{ multivariate }%-- one or more ...
