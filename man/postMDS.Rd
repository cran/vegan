\name{postMDS}
\alias{postMDS}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Scaling of Multi-Dimensional Scaling Results }
\description{
  Function performs some post-standardizations for multidimensional
  scaling, in order to make the configurations easier to interpret.
}
\usage{
postMDS(x, dist, pc=TRUE, center=TRUE, halfchange=TRUE, threshold=0.8, nthreshold=10)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{Configuration from multidimensional scaling. }
  \item{dist}{Dissimilarity matrix used in multidimensional scaling. }
  \item{pc}{Rotate to principal components. }
  \item{center}{Centre the configuration. }
  \item{halfchange}{Scale axes to half-change units.}
  \item{threshold}{Largest dissimilarity used in half-change scaling. }
  \item{nthreshold}{ Minimum number of points in half-change scaling. }
}
\details{
  Multidimensional scaling has no internal, natural scaling or
  orientation of axes, and so similar configurations may look
  quite different.  The function offers some standard methods that make
  configurations easier to compare.

  Centring moves the origin to the
  average of each axis.  Principal components rotate the configuration
  so that the variance of points is maximized on first dimensions.

  Half-change scaling scales the configuration so that one
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
  Standardized configuration (an array: stress and attributes are dropped).
}

\author{Jari Oksanen, based on unpublished method by Peter Minchin. }

\seealso{ \code{\link[mva]{dist}}, \code{\link{vegdist}}, \code{\link[MASS]{isoMDS}} }

\examples{
library(mva)
library(MASS)
data(varespec)
vare.dist <- vegdist(varespec)
mds.null <- isoMDS(vare.dist)
\dontrun{par(mfrow=c(1,2))}
\dontrun{eqscplot(mds.null$points)}
mds.post <- postMDS(mds.null$points, vare.dist)
\dontrun{eqscplot(mds.post)}
}
\keyword{multivariate }%-- one or more ...
