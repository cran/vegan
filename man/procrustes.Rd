\name{procrustes}
\alias{procrustes}
\alias{print.procrustes}
\alias{summary.procrustes}
\alias{print.summary.procrustes}
\alias{plot.procrustes}
\alias{residuals.procrustes}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Procrustes Rotation of Two Configurations }
\description{
  Rotates a configuration to maximum similarity
  with another configuration.
}
\usage{
procrustes(X, Y, scale=TRUE)
%\method{print}{procrustes}(x, ...)
\method{summary}{procrustes}(object, ...)
%\method{print}{summary.procrustes}(x, ...)
\method{plot}{procrustes}(x, kind=1, ...)
\method{residuals}{procrustes}(object)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{X}{Target matrix.}
  \item{Y}{Matrix to be rotated.}
  \item{scale}{Allow scaling of axes of Y.}
  \item{x}{An object of class \code{procrustes}.}
  \item{object}{An object of class \code{procrustes}.}
  \item{kind}{The kind of plot produced: \code{kind=1} plots shifts in
    two configurations and \code{kind=2} plots an impulse diagram of
    residuals. }
  \item{...}{Parameters passed to plotting functions}
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

\seealso{\code{\link[MASS]{isoMDS}}, \code{\link{initMDS}},
  \code{\link[MASS]{eqscplot}} } 

\examples{
library(MASS)
library(mva)
data(varespec)
vare.dist <- vegdist(wisconsin(varespec))
mds.null <- isoMDS(vare.dist)
## This was a good seed for me: your rng may vary.
## Reset your rng if you run this example.
set.seed(237)
mds.alt <- isoMDS(vare.dist, initMDS(vare.dist))
vare.proc <- procrustes(mds.alt$points, mds.null$points)
vare.proc
summary(vare.proc)
\dontrun{plot(vare.proc)}
\dontrun{plot(vare.proc, kind=2)}
residuals(vare.proc)
}
\keyword{multivariate }%-- one or more ...
