\name{rankindex}
\alias{rankindex}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Compares Dissimilarity Indices for Gradient Detection }
\description{
  Rank correlations between dissimilarity indices
  and gradient separation.
}
\usage{
rankindex(grad, veg, method="kendall")
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{grad}{The gradient variable or matrix. }
  \item{veg}{The community data matrix. }
  \item{method}{Rank correlation method used. }
}
\details{
  A good dissimilarity index for multidimensional scaling 
  should have a high rank-order similarity with gradient separation.
  The function compares all indices in \code{\link{vegdist}} against
  gradient separation using rank correlation coefficients in
  \code{\link[ctest]{cor.test}}. 
}
\value{
  Returns a named vector of rank correlations.
}
\references{ Faith, F.P., Minchin, P.R. and Belbin,
  L. (1987).  Compositional dissimilarity as a robust measure of
    ecological distance. \emph{Vegetatio} 69, 57-68. }
\author{Jari Oksanen }
\note{
  There are several problems in using rank correlation coefficients.
  Typically there are very many ties when \eqn{n(n-1)/2} gradient
  separation values are derived from just \eqn{n} observations.
  Due to floating point arithmetics, many tied values differ by
  machine epsilon and are arbitrarily ranked differently by
  \code{\link{rank}} used in \code{\link[ctest]{cor.test}}.  Two indices
  which are identical with certain
  transformation or standardization may differ slightly
  (magnitude \eqn{10^{-15}}) and this may lead into third or fourth decimal
  instability in rank correlations.  Small differences in rank
  correlations should not be taken too seriously.  Probably this method
  should be replaced with a sounder method, but I do not yet know
  which\ldots 

  Kendall's rank correlation may be very slow with large data sets and
  you may consider
  other alternatives in \code{\link[ctest]{cor.test}}.
}


\seealso{\code{\link{vegdist}}, \code{\link[MASS]{isoMDS}},
    \code{\link[ctest]{cor.test}}, \code{\link{Machine}}  }

\examples{
data(varespec)
data(varechem)
## The next scales all environmental variables to unit variance.
## Some would use PCA transformation.
rankindex(scale(varechem), varespec)
rankindex(scale(varechem), wisconsin(varespec))
}
\keyword{ multivariate }%-- one or more ...
