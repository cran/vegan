\name{mantel}
\alias{mantel}
\alias{print.mantel}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Mantel Test for Two Dissimilarity Matrices }
\description{
  Function \code{mantel} finds the Mantel statistic as a matrix
  correlation between two dissimilarity matrices, and evaluates the
  significance of the statistic by permuting rows and columns of the
  dissimilarity matrix.  The statistic is evaluated either as a moment
  correlation or as a rank correlation.

}
\usage{
mantel(xdis, ydis, method="pearson", permutations=1000, strata)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{xdis}{ First dissimilarity matrix or a \code{dist} object. }
  \item{ydis}{ Second dissimilarity matrix or a \code{dist} object. }
  \item{method}{ Correlation method, as accepted by \code{cor.test}:
    \code{pearson}, \code{spearman} or \code{kendall}. }
  \item{permutations}{Number of permutations in assessing significance. }
  \item{strata}{An integer vector or factor specifying the strata for
    permutation. If supplied, observations are permuted only within the
    specified strata.}
}
\details{
  Mantel statistic is simply a correlation between entries of two
  dissimilarity matrices (some use cross products, but these are linearly
  related).  However, the significance cannot be directly assessed,
  because there are \eqn{N(N-1)/2} entries for just \eqn{N} observations.
  Mantel developed asymptotic test, but here we use permutations of
  \eqn{N} rows and columns of dissimilarity matrix.

  The function uses \code{\link{cor.test}}, which should accept
  alternatives \code{pearson} for product moment correlations and
  \code{spearman} or \code{kendall} for rank correlations.
}
\value{
  The function returns a list of class \code{mantel} with following
  components: 
  \item{Call }{Function call.}
  \item{method }{Correlation method used, as returned by
    \code{\link{cor.test}}.}
  \item{statistic}{The Mantel statistic.}
  \item{signif}{Empirical significance level from permutations.}
  \item{perm}{A vector of permuted values.}
  \item{permutations}{Number of permutations.}
  }
  
}
\references{ The test is due to Mantel, of course, but the
  current implementation is based on Legendre and Legendre.

  Legendre, P. and Legendre, L. (1998) \emph{Numerical Ecology}. 2nd English
  Edition. Elsevier.
  
}

\author{Jari Oksanen }

\note{The function requires package \code{mva} for handling
  dissimilarity matrices.
  }

\seealso{\code{\link{cor.test}} for correlation tests,
  \code{\link{protest}} (``Procrustes test'') for an alternative with
  ordination diagrams,
  and \code{\link{anosim}} for comparing dissimilarities against
  classification.  For dissimilarity matrices, see \code{\link{vegdist}}
  or \code{\link[mva]{dist}}. }

\examples{
## Is vegetation related to environment?
data(varespec)
data(varechem)
veg.dist <- vegdist(varespec) # Bray-Curtis
env.dist <- vegdist(scale(varechem), "euclid")
mantel(veg.dist, env.dist)
mantel(veg.dist, env.dist, method="spear")
}
\keyword{ multivariate }
\keyword{ htest }
