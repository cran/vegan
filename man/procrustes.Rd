\name{procrustes}
\alias{procrustes}
\alias{print.procrustes}
\alias{summary.procrustes}
\alias{print.summary.procrustes}
\alias{plot.procrustes}
\alias{residuals.procrustes}
\alias{fitted.procrustes}
\alias{protest}
\alias{print.protest}

\title{Procrustes Rotation of Two Configurations }
\description{
  Rotates a configuration to maximum similarity
  with another configuration.
}
\usage{
procrustes(X, Y, scale = TRUE, symmetric = FALSE)
\method{summary}{procrustes}(object, ...)
\method{plot}{procrustes}(x, kind=1, choices=c(1,2), xlab, ylab, main, ...)
\method{residuals}{procrustes}(object, ...)
\method{fitted}{procrustes}(object, truemean = TRUE, ...)
protest(X, Y, permutations = 1000, strata)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{X}{Target matrix}
  \item{Y}{Matrix to be rotated.}
  \item{scale}{Allow scaling of axes of \code{Y}.}
  \item{symmetric}{Use symmetric Procrustes statistic (the rotation will
    still be non-symmetric).}
  \item{x}{An object of class \code{procrustes}.}
  \item{object}{An object of class \code{procrustes}.}
  \item{kind}{The kind of plot produced: \code{kind=1} plots shifts in
    two configurations and \code{kind=2} plots an impulse diagram of
    residuals. }
  \item{choices}{Axes (dimensions) plotted.}
  \item{xlab, ylab}{Axis labels, if defaults unacceptable.}
  \item{main}{Plot title, if default unacceptable.}
  \item{truemean}{Use the original range of target matrix instead of
    centring the fitted values.}
  \item{permutations}{Number of permutation to assess the significance
    of the symmetric Procrustes statistic. }
  \item{strata}{An integer vector or factor specifying the strata for
    permutation. If supplied, observations are permuted only within the
    specified strata.}
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
  to fit \code{X}, the scaling is non-symmetric. However, with
  \code{symmetric=TRUE}, the configurations are scaled to equal
  dispersions and  a symmetric version of the Procrustes statistic
  is computed.

  Instead of matrix, \code{X} and \code{Y} can be results from an
  ordination form which \code{\link{scores}} can extract results.
  Function \code{plot} plots a \code{procrustes}
  object and \code{residuals} returns the pointwise
  residuals, and \code{fitted} the fitted values, either centred to zero
  mean (if \code{truemean=FALSE}) or with the original scale (these
  hardly make sense if \code{symmetric = TRUE}). In
  addition, there are \code{summary} and \code{print} methods.

  If matrix \code{X} has a lower number of columns than matrix
  \code{Y}, then matrix \code{X} will be filled with zero columns to
  match dimensions. This means that the function can be used to rotate
  an ordination configuration to an environmental variable (most
  practically extracting the result with the \code{fitted} function).

  Function \code{protest} calls \code{procrustes(..., symmetric = TRUE)}
  repeatedly to estimate the `significance' of the Procrustes
  statistic. Function \code{protest} uses a correlation-like statistic
  derived from the symmetric Procrustes sum of squares \eqn{ss} as
  \eqn{r =\sqrt{(1-ss)}}, and sometimes called \eqn{m_{12}}. Function
  \code{protest} has own \code{print} method, but otherwise uses
  \code{procrustes} methods. Thus \code{plot} with a \code{protest} object
  yields a ``Procrustean superimposition plot.''
}

\value{
  Function \code{procrustes} returns an object of class
  \code{procrustes} with items. Function \code{protest} inherits from
  \code{procrustes}, but amends that with some new items:
  \item{Yrot }{Rotated matrix \code{Y}.}
  \item{X}{Target matrix.}
  \item{ss }{Sum of squared differences between \code{X} and \code{Yrot}.}
  \item{rotation}{Orthogonal rotation matrix.}
  \item{translation}{Translation of the origin.}
  \item{scale}{Scaling factor.}
  \item{symmetric}{Type of \code{ss} statistic.}
  \item{call}{Function call.}
  \item{t0}{This and the following items are only in class
    \code{protest}:  Procrustes correlation from non-permuted solution.}
  \item{t}{Procrustes correlations from permutations.}
  \item{signif}{`Significance' of \code{t}}
  \item{permutations}{Number of permutations.}
  \item{strata}{The name of the stratifying variable.}
  \item{stratum.values}{Values of the stratifying variable.}
}
\references{
  Mardia, K.V., Kent, J.T. and Bibby,
  J.M. (1979). \emph{Multivariate Analysis}. Academic Press.

  Peres-Neto, P.R. and Jackson, D.A. (2001). How well do multivariate
  data sets match? The advantages of a Procrustean superimposition
  approach over the Mantel test. \emph{Oecologia} 129: 169-178.
  
}
\author{Jari Oksanen }

\note{The function \code{protest} follows Peres-Neto & Jackson (2001),
  but its implementation is still after Mardia \emph{et al.}
  (1979). Peres-Neto & Jackson standardized the dimensions to unit
  variance which distorts the configuration, whereas \code{protest} uses
  original configuration.  }

\seealso{\code{\link[MASS]{isoMDS}}, \code{\link{initMDS}} for obtaining
objects for \code{procrustes}, and \code{\link{mantel}} for an
alternative to \code{protest} without need of dimension reduction.} 

\examples{
data(varespec)
vare.dist <- vegdist(wisconsin(varespec))
library(MASS)  ## isoMDS
library(mva)   ## cmdscale to start isoMDS
mds.null <- isoMDS(vare.dist, tol=1e-7)
## This was a good seed for me: your rng may vary.
set.seed(237)
mds.alt <- isoMDS(vare.dist, initMDS(vare.dist), maxit=200, tol=1e-7)
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
\keyword{htest}