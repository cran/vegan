\name{vegdist}
\alias{vegdist}
\title{Dissimilarity Indices for Community Ecologists }
\description{
  The function computes dissimilarity indices that are useful for or
  popular with community ecologists.
  Gower, Bray--Curtis, Jaccard and
  Kulczynski indices are good in detecting underlying
  ecological gradients (Faith et al. 1987). Morisita and Horn--Morisita
  indices should be able to handle different sample sizes (Wolda 1981),
  and Mountford (1962) index for presence--absence data should
  be able to handle unknown (and variable) sample sizes.
}

\usage{ vegdist(x, method="bray", diag=FALSE, upper=FALSE) } 
\arguments{
  \item{x}{ Community data matrix.}
  \item{method}{Dissimilarity index, partial match to  \code{"manhattan"},
    \code{"euclidean"}, \code{"canberra"}, \code{"bray"}, \code{"kulczynski"},
     \code{"jaccard"}, \code{"gower"}, \code{"morisita"}, \code{"horn"} or
    \code{"mountford"}.}
  \item{diag}{Compute diagonals. }
  \item{upper}{Return only the upper diagonal. }
}
\details{
  Jaccard and Mountford indices are discussed below.
  The other indices are defined as:
  \tabular{ll}{
    \code{euclidean}
    \tab \eqn{d_{jk} = \sqrt{\sum_i (x_{ij}-x_{ik})^2}}{d[jk] = sqrt(sum (x[ij]-x[ik])^2)}
    \cr
    \code{manhattan}
    \tab \eqn{d_{jk} = \sum_i |x_{ij} - x_{ik}|}{d[jk] = sum(abs(x[ij] -
      x[ik]))}
    \cr
    \code{gower}
    \tab \eqn{d_{jk} = \sum_i \frac{|x_{ij}-x_{ik}|}{\max x_i-\min x_i}}{d[jk] = sum (abs(x[ij]-x[ik])/(max(x[i])-min(x[i]))}
    \cr
    \code{canberra}
    \tab \eqn{d_{jk}=\frac{1}{N-Z} \sum_i
      \frac{|x_{ij}-x_{ik}|}{x_{ij}+x_{ik}}}{d[jk] = (1/NZ) sum
      ((x[ij]-x[ik])/(x[ij]+x[ik]))}
    \cr
    \tab where \eqn{NZ} is the number of non-zero entries.
    \cr
    \code{bray}
    \tab \eqn{d_{jk} = \frac{\sum_i |x_{ij}-x_{ik}|}{\sum_i (x_{ij}+x_{ik})}}{d[jk] = (sum abs(x[ij]-x[ik])/(sum (x[ij]+x[ik]))}
    \cr
    \code{kulczynski}
    \tab \eqn{d_{jk} = 1-0.5(\frac{\sum_i \min(x_{ij},x_{ik})}{\sum_i x_{ij}} +
      \frac{\sum_i \min(x_{ij},x_{ik})}{\sum_i x_{ik}} )}{d[jk] 1 - 0.5*((sum min(x[ij],x[ik])/(sum x[ij]) + (sum
      min(x[ij],x[ik])/(sum x[ik]))}
    \cr
    \code{morisita}
    \tab {\eqn{d_{jk} = \frac{2 \sum_i x_{ij} x_{ik}}{(\lambda_j +
	  \lambda_k) \sum_i x_{ij} \sum_i
	  x_{ik}}}{d[jk] = 2*sum(x[ij]*x[ik])/((lambda[j]+lambda[k]) *
	sum(x[ij])*sum(x[ik]))}  }
    \cr
    \tab where \eqn{\lambda_j = \frac{\sum_i x_{ij} (x_{ij} - 1)}{\sum_i
      x_{ij} \sum_i (x_{ij} - 1)}}{lambda[j] =
      sum(x[ij]*(x[ij]-1))/sum(x[ij])*sum(x[ij]-1)}
    \cr
    \code{horn}
    \tab Like \code{morisita}, but \eqn{\lambda_j = \sum_i
      x_{ij}^2/(\sum_i x_{ij})^2}{lambda[j] = sum(x[ij]^2)/(sum(x[ij])^2)}
  }

  Jaccard index is computed as \eqn{2B/(1+B)}, where \eqn{B} is
  Bray--Curtis dissimilarity.

  Mountford index is defined as \eqn{M = 1/\alpha} where \eqn{\alpha} is
  the parameter of Fisher's logseries assuming that the compared
  communities are samples from the same community
  (cf. \code{\link{fisherfit}}, \code{\link{fisher.alpha}}). The index
  \eqn{M} is found as the positive root of equation \eqn{\exp(aM) +
  \exp(bM) = 1 + \exp[(a+b-j)M]}{exp(a*M) + exp(b*M) = 1 +
  exp((a+b-j)*M)}, where \eqn{j} is the number of species occurring in
  both communities, and \eqn{a} and \eqn{b} are the number of species in
  each separate community (so the index uses presence--absence
  information). Mountford index is usually misrepresented in the
  literature: indeed Mountford (1962) suggested an approximation to be used as starting
  value in iterations, but the proper index is defined as the root of the equation
  above. The function \code{vegdist} solves \eqn{M} with the Newton
  method. Please note that if either \eqn{a} or \eqn{b} are equal to
  \eqn{j}, one of the communities could be a subset of other, and the
  dissimilarity is \eqn{0} meaning that non-identical objects may be
  regarded as similar and the index is non-metric. The Mountford index
  is in the range \eqn{0 \dots \log(2)}, but the dissimilarities are
  divided by \eqn{\log(2)} 
  so that the results will be in the conventional range \eqn{0 \dots 1}.

  Morisita index can be used with genuine count data only. Its
  Horn--Morisita variant is able to handle any abundance data.

  Euclidean and Manhattan dissimilarities are not good in gradient
  separation without proper standardization but are still included for
  comparison and special needs.

  Bray--Curtis and Jaccard indices are rank-order similar, and some
  other indices become identical or rank-order similar after some 
  standardizations, especially with presence/absence transformation of
  equalizing site totals with \code{\link{decostand}}.

  The naming conventions vary. The one adopted here is traditional
  rather than truthful to priority. The abbreviation \code{"horn"} for
  the Horn--Morisita index is misleading, since there is a separate
  Horn index. The abbreviation will be changed if that index is implemented in
  \code{vegan}. 
}
\value{
  Should provide a drop-in replacement for \code{\link[mva]{dist}} and
  return a distance object of the same type. 
}
\references{
  Faith, D.P, Minchin, P.R. and Belbin, L. (1987).
  Compositional dissimilarity as a robust measure of ecological
  distance. \emph{Vegetatio} 69, 57--68.

  Mountford, M. D. (1962). An index of similarity and its application to
  classification problems. In: P.W.Murphy (ed.),
  \emph{Progress in Soil Zoology}, 43--50. Butterworths.

  Wolda, H. (1981). Similarity indices, sample size and
  diversity. \emph{Oecologia} 50, 296--302.
}

\author{ Jari Oksanen }

\note{The  function is an alternative to \code{\link[mva]{dist}} adding
  some ecologically meaningful indices.  Both methods should produce
  similar types of objects which can be interchanged in any method
  accepting either.  Manhattan and Euclidean dissimilarities should be
  identical in both methods, and Canberra dissimilarity may be similar.
}

\seealso{ \code{\link{decostand}}, \code{\link[mva]{dist}},
  \code{\link{rankindex}}, \code{\link[MASS]{isoMDS}}, \code{\link{stepacross}}. }

\examples{
data(varespec)
vare.dist <- vegdist(varespec)
# Orlóci's Chord distance: range 0 .. sqrt(2)
vare.dist <- vegdist(decostand(varespec, "norm"), "euclidean")
}
\keyword{ multivariate }
