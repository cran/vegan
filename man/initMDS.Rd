\name{initMDS}
\alias{initMDS}
\title{ Random Start for isoMDS }
\description{
  Random start for multidimensional scaling.
}
\usage{
initMDS(x, k=2)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{Dissimilarity matrix for isoMDS.}
  \item{k}{Number of dimensions.}
}
\details{
  Returns a random configuration for multidimensional scaling.  The
  default practice in \code{\link[MASS]{isoMDS}} is to start with metric
  scaling, but this
  is easily trapped in a local minimum.  Thus the recommended way
  is to use several random starts and select a solution with
  lowest stress and similar configuration to other solutions with about
  similar stress. }

\value{
  A random configuration which is intended to be used within
  \code{\link[MASS]{isoMDS}} only.
}  
\references{ Minchin, P.R. (1987)  An evaluation of relative robustness
  of techniques for ecological ordinations. \emph{Vegetatio} 71, 145-156. }

\author{ Jari Oksanen }

\seealso{\code{\link[MASS]{isoMDS}}, \code{\link[mva]{cmdscale}},
  \code{\link{procrustes}}.  }

\examples{
library(mva)
library(MASS)
data(varespec)
vare.dist <- vegdist(varespec, "bray")
mds.null <- isoMDS(vare.dist)
## See if you can get any better
## (This is a small data set, and an easy case: probably you can't
## improve much).
stress <- rep(NA,10)
for (i in 1:10)
  stress[i] <- isoMDS(vare.dist, initMDS(vare.dist), maxit=200, trace=F)
}
\keyword{ multivariate }%-- one or more ...
