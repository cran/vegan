\name{envfit}
\alias{envfit}
\alias{vectorfit}
\alias{factorfit}
\alias{plot.envfit}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Fits an Environmental Vector or Factor onto an Ordination }
\description{
  The function fits environmental vectors or factors onto an
  ordination. The projection of points onto vectors have
  maximum correlations with corresponding environmental variables, and
  the factors show the averages of factor levels.
}
\usage{
envfit(X, P)
\method{plot}{envfit}(x, choices = c(1,2), arrow.mul = 1, col = "blue", add = TRUE, ...)
vectorfit(X, P)
factorfit(X, P)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{X}{ Ordination configuration.}
  \item{P}{ Matrix or vector of environmental variable(s). }
  \item{x}{A result object from \code{envfit}.}
  \item{choices}{Axes to plotted.}
  \item{arrow.mul}{Multiplier for vector lenghts.}
  \item{col}{Colour in plotting.}
  \item{add}{Results added to an existing ordination plot.}
  \item{...}{Parameters to \code{text} function.}
}
\details{
  Function \code{envfit} finds vectors or factor averages of
  environmental variabels.  Function \code{plot.envfit} adds these in an
  ordination diagram.  If \code{X} is a \code{\link{data.frame}},
  \code{envfit}
  uses \code{factorfit} for \code{\link{factor}} variables and
  \code{vectorfit} for other variables.  If \code{X} is a matrix or a
  vector, \code{envfit} uses only \code{vectrofit}.
  
  Functions \code{vectorfit} and \code{factorfit} can be called directly.
  Function \code{vectorfit} finds directions in the ordination space
  towards which the environmental vectors change most rapidly and to
  which they have maximal correlations with the ordination
  configuration.  Function \code{factorfit} finds means of ordination
  scores for factor levels.
}
\value{
  Function \code{envfit} returns
  an object of class \code{envfit} with items:
  \item{vectors}{Result of \code{vectorfit}.}
  \item{factors}{Result of \code{factorfit}.}
  Function \code{vectorfit} returns a matrix where rows correspond to
  environmental variables and
  columns give the
  endpoints of environmental vectors (or their direction cosines), plus
  an extra column that
  gives the correlation coefficient to the fitted vector.
  The vectors are of unit length.  Function \code{factorfit} returns a
  matrix where rows are factor levels.
  Function \code{plot.envfit} scales the vectors by correlation.
}

\author{Jari Oksanen }

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
  abound in \R, and \code{\link{surf}} provides a wrapper for some.
  }

\seealso{
  A better alternative to vectors may be \code{\link{surf}}.    
  }

\examples{
data(varespec)
data(varechem)
library(MASS)
library(mva)
vare.dist <- vegdist(wisconsin(varespec))
vare.mds <- isoMDS(vare.dist)
vare.mds <- postMDS(vare.mds, vare.dist)
vare.fit <- envfit(vare.mds$points, varechem)
plot(vare.mds$points, pch="+", asp=1, xlab="Dim1", ylab="Dim2")
plot(vare.fit)
}
\keyword{multivariate }%-- one or more ...
\keyword{aplot}




