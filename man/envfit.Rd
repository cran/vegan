\name{envfit}
\alias{envfit}
\alias{vectorfit}
\alias{factorfit}
\alias{plot.envfit}
\alias{print.envfit}
\alias{print.factorfit}
\alias{print.vectorfit}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Fits an Environmental Vector or Factor onto an Ordination }
\description{
  The function fits environmental vectors or factors onto an
  ordination. The projection of points onto vectors have
  maximum correlations with corresponding environmental variables, and
  the factors show the averages of factor levels.
}
\usage{
envfit(X, P, permutations = 0, strata, choices=c(1,2))
\method{plot}{envfit}(x, choices = c(1,2), arrow.mul = 1, col = "blue", add = TRUE, ...)
vectorfit(X, P, permutations = 0, strata, choices=c(1,2))
factorfit(X, P, permutations = 0, strata, choices=c(1,2))
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{X}{ Ordination configuration.}
  \item{P}{ Matrix or vector of environmental variable(s). }
  \item{permutations}{ Number of permutations for assessing significance
    of vectors or factors.}
  \item{x}{A result object from \code{envfit}.}
  \item{choices}{Axes to plotted.}
  \item{arrow.mul}{Multiplier for vector lengths.}
  \item{col}{Colour in plotting.}
  \item{add}{Results added to an existing ordination plot.}
  \item{strata}{An integer vector or factor specifying the strata for
    permutation. If supplied, observations are permuted only within the
    specified strata.}
  \item{...}{Parameters to \code{text} function.}
}
\details{
  Function \code{envfit} finds vectors or factor averages of
  environmental variables.  Function \code{plot.envfit} adds these in an
  ordination diagram.  If \code{X} is a \code{\link{data.frame}},
  \code{envfit}
  uses \code{factorfit} for \code{\link{factor}} variables and
  \code{vectorfit} for other variables.  If \code{X} is a matrix or a
  vector, \code{envfit} uses only \code{vectorfit}.
  
  Functions \code{vectorfit} and \code{factorfit} can be called directly.
  Function \code{vectorfit} finds directions in the ordination space
  towards which the environmental vectors change most rapidly and to
  which they have maximal correlations with the ordination
  configuration.  Function \code{factorfit} finds averages of ordination
  scores for factor levels.

  If \code{permutations} \eqn{> 0}, the `significance' of fitted vectors
  or factors is assessed using permutation of environmental variables.
  The goodness of fit statistic is squared correlation coefficient
  (\eqn{r^2}).
  For factors this is defined as \eqn{r^2 = 1 - ss_w/ss_t}, where
  \eqn{ss_w} and \eqn{ss_t} are within-group and total sums of squares.
}
\value{
  Functions \code{vectorfit} and \code{factorfit} return lists of
  classes \code{vectorfit} and \code{factorfit} which have a
  \code{print} method.  The result object have the following items:

  \item{arrows}{Arrow endpoints from \code{vectorfit}. The arrows are
    scaled to unit length.}
  \item{centroids}{Class centroids from \code{factorfit}.}
  \item{r}{Goodness of fit statistic: Squared orrelation coefficient}
  \item{permutations}{Number of permutations.}
  \item{pvals}{Empirical P-values for each variable.}

  Function \code{envfit} returns a list of class \code{envfit} with
  results of \code{vectorfit} and \code{envfit} as items.
  
  Function \code{plot.envfit} scales the vectors by correlation.
}

\author{Jari Oksanen.  The permutation test derives from the code
  suggested by Michael Scroggie. }

\note{
  Fitted vectors have become the method of choice in displaying
  environmental variables in ordination.  Indeed, they are the optimal
  way of presenting environmental variables in Constrained
  Correspondence Analysis \code{\link{cca}}, since there they are the
  linear constraints.
  In unconstrained ordination the relation between external variables
  and ordination configuration may be less linear, and therefore other
  methods than arrows may be more useful.  The simplest is to adjust the
  plotting symbol sizes (\code{cex}, \code{\link{symbols}}) by
  environmental variables.
  More fancy methods involve smoothing and regression methods that
  abound in \R, and \code{\link{ordisurf}} provides a wrapper for some.
  }

\seealso{
  A better alternative to vectors may be \code{\link{ordisurf}}.    
  }

\examples{
data(varespec)
data(varechem)
library(MASS)
library(mva)
vare.dist <- vegdist(wisconsin(varespec))
vare.mds <- isoMDS(vare.dist)
vare.mds <- postMDS(vare.mds, vare.dist)
vare.fit <- envfit(vare.mds$points, varechem, 1000)
vare.fit
plot(vare.mds$points, pch="+", asp=1, xlab="Dim1", ylab="Dim2")
plot(vare.fit)
}
\keyword{multivariate }%-- one or more ...
\keyword{aplot}
\keyword{htest}




