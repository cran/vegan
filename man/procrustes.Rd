\name{procrustes}
\alias{procrustes}
\alias{print.procrustes}
\alias{summary.procrustes}
\alias{print.summary.procrustes}
\alias{plot.procrustes}
\alias{residuals.procrustes}

\title{Procrustes Rotation of Two Configurations }
\description{
  Rotates a configuration to maximum similarity
  with another configuration.
}
\usage{
procrustes(X, Y, scale=TRUE)
\method{summary}{procrustes}(object, ...)
\method{plot}{procrustes}(x, kind=1, axes=c(1,2), xlab, ylab, main, ...)
\method{residuals}{procrustes}(object, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{X}{Target matrix}
  \item{Y}{Matrix to be rotated.}
  \item{scale}{Allow scaling of axes of \code{Y}.}
  \item{x}{An object of class \code{procrustes}.}
  \item{object}{An object of class \code{procrustes}.}
  \item{kind}{The kind of plot produced: \code{kind=1} plots shifts in
    two configurations and \code{kind=2} plots an impulse diagram of
    residuals. }
  \item{axes}{Axes (dimensions) plotted.}
  \item{xlab, ylab}{Axis labels, if defaults unacceptable.}
  \item{main}{Plot title, if default unacceptable.}
  \item{...}{Other parameters passed to functions.}
}
\details{
  Procrustes rotation rotates a matrix to maximum similarity with a
  target matrix minimizing sum of squared differences.  Procrustes
  rotation is typically used in comparison of ordination results.  It is
  particularly useful in comparing alternative solutions in
  multidimensional scaling.  If \code{scale=FALSE}, the function only
  rotates matrix \code{Y}. If \code{scale=TRUE} , it scales linearly
  configuration \code{Y} for maximum similarity.  Since \code{Y} is scaled
  to fit \code{X}, the scaling is non-symmetric.

  Instead of matrix, \code{X} and \code{Y} can be results from an
  ordination form which \code{\link{scores}} can extract results.
  Function \code{plot.procrustes} plots a \code{procrustes}
  object and \code{residuals.procrustes} returns the pointwise
  residuals. In addition, there are \code{summary} and \code{print}
  methods. 
}

\value{
  Function \code{procrustes} returns an object of class
\code{procrustes} with items:
  \item{Yrot }{Rotated matrix \code{Y}.}
  \item{X}{Target matrix.}
  \item{ss }{Sum of squared differences between \code{X} and \code{Yrot}.}
  \item{rotation}{Orthogonal rotation matrix.}
  \item{translation}{Translation of the origin.}
  \item{scale}{Scaling factor.}
  \item{call}{Function call.}
}
\references{Mardia, K.V., Kent, J.T. and Bibby,
  J.M. (1979). Multivariate Analysis. Academic Press. }
\author{Jari Oksanen }

\seealso{\code{\link[MASS]{isoMDS}}, \code{\link{initMDS}} } 

\examples{
data(varespec)
vare.dist <- vegdist(wisconsin(varespec))
library(MASS)  ## isoMDS
library(mva)   ## cmdscale to start isoMDS
mds.null <- isoMDS(vare.dist)
## This was a good seed for me: your rng may vary.
set.seed(237)
mds.alt <- isoMDS(vare.dist, initMDS(vare.dist), maxit=200)
vare.proc <- procrustes(mds.alt$points, mds.null$points)
vare.proc
summary(vare.proc)
plot(vare.proc)
plot(vare.proc, kind=2)
residuals(vare.proc)
## Reset rng:
rm(.Random.seed)
}
\keyword{multivariate }%-- one or more ...
