\name{vegdist}
\alias{vegdist}
\title{Good Dissimilarity Measures for Ecological Gradients }
\description{
  The function computes community dissimilarity indices which are known
  to have a good rank-order relation with gradient separation and are
  thus efficient in community ordination with multidimensional scaling.
}
\usage{
vegdist(x, method="bray", diag=FALSE, upper=FALSE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{ Community data matrix}
  \item{method}{Dissimilarity index }
  \item{diag}{Compute diagonals. }
  \item{upper}{Return only the upper diagonal. }
}
\details{
  The function knows the following dissimilarity indices:
  \tabular{ll}{
    \code{euclidean}
    \tab \eqn{d_{jk} = \sqrt{\sum_i (x_{ij}-x_{ik})^2}}{d[jk] = sqrt(sum (x[ij]-x[ik])^2)}
    \cr
    \code{manhattan}
    \tab \eqn{d_{jk} = \sum_i |x_{ij} - x_{ik}|}{d[jk] = sum(abs(x[ij] -
      x[ik]))}
    \cr
    \code{gower}
    \tab \eqn{d_{jk} = \sum_i \frac{|x_{ij}-x_{ik}|}{\max_i-\min_i}}{d[jk] = sum (abs(x[ij]-x[ik])/(max(i)-min(i))}
    \cr
    \code{canberra}
    \tab \eqn{d_{jk}=\frac{1}{N-Z} \sum_i
      \frac{|x_{ij}-x_{ik}|}{x_{ij}+x_{ik}}}{d[jk] = (1/NZ) sum ((x[ij]-x[ik])/(x[ij]+x[ik]))}
    \cr
    \code{bray}
    \tab \eqn{d_{jk} = \frac{\sum_i |x_{ij}-x_{ik}|}{\sum_i (x_{ij}+x_{ik})}}{d[jk] = (sum abs(x[ij]-x[ik])/(sum (x[ij]+x[ik]))}
    \cr
    \code{kulczynski}
    \tab \eqn{d_{jk} = 1-0.5(\frac{\sum_i \min(x_{ij},x_{ik})}{\sum_i x_{ij}} +
      \frac{\sum_i \min(x_{ij},x_{ik})}{\sum_i x_{ik}} )}{d[jk] 1 - 0.5*((sum min(x[ij],x[ik])/(sum x[ij]) + (sum
      min(x[ij],x[ik])/(sum x[ik]))}
  }
  where \eqn{N-Z}{NZ} is the number of non-zero entries.

  Infamous ``double zeros'' are removed in Canberra dissimilarity.

  Euclidean and Manhattan dissimilarities are not good in gradient
  separation without proper standardization but are still included for
  comparison and special needs.

  Some of indices become identical or rank-order similar after some
  standardizations. 
}
\value{
  Should provide a drop-in replacement for \code{\link[mva]{dist}} and
  return a distance object of the same type. 
}
\references{ Faith, D.P, Minchin, P.R. and Belbin, L. (1987)
  Compositional dissimilarity as a robust measure of ecological
  distance. \emph{Vegetatio} 69, 57-68. }
\author{ Jari Oksanen }
\note{The  function is an alternative to \code{\link[mva]{dist}} adding
  some ecologically meaningful indices.  Both methods should produce
  similar types of objects which can be interchanged in any method
  accepting either.  Manhattan and Euclidean dissimilarities should be
  identical in both methods, and Canberra dissimilarity may be similar.

  Function \code{vegdist} does not \code{\link{require}} package
  \code{mva} as such, but handling of its result object may be difficult
  without support functions in \code{mva}.
}


\seealso{ \code{\link{decostand}}, \code{\link[mva]{dist}},
  \code{\link{rankindex}}, \code{\link[MASS]{isoMDS}} }

\examples{
data(varespec)
vare.dist <- vegdist(varespec)
}
\keyword{ multivariate }%-- one or more ...
