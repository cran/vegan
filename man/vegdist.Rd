\name{vegdist}
\alias{vegdist}
\title{Dissimilarity Indices for Community Ecologists }
\description{
  The function computes dissimilarity indices that are useful for or
  popular with community ecologists.
  Gower, Bray--Curtis, Jaccard and
  Kulczynski indices are good in detecting underlying
  ecological gradients (Faith et al. 1987). Morisita, Horn--Morisita and Binomial
  indices should be able to handle different sample sizes (Wolda 1981,
  Krebs 1999, Anderson & Millar 2004),
  and Mountford (1962) and Raup-Crick indices for presence--absence data should
  be able to handle unknown (and variable) sample sizes.
}

\usage{vegdist(x, method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
        na.rm = FALSE, ...) } 
\arguments{
  \item{x}{Community data matrix.}
  \item{method}{Dissimilarity index, partial match to  \code{"manhattan"},
    \code{"euclidean"}, \code{"canberra"}, \code{"bray"}, \code{"kulczynski"},
     \code{"jaccard"}, \code{"gower"}, \code{"morisita"}, \code{"horn"},
     \code{"mountford"}, \code{"raup"} or \code{"binomial"}.}
  \item{binary}{Perform presence/absence standardization before analysis
    using \code{\link{decostand}}.}
  \item{diag}{Compute diagonals. }
  \item{upper}{Return only the upper diagonal. }
  \item{na.rm}{Pairwise deletion of missing observations when
    computing dissimilarities.}
  \item{\dots}{Other parameters.  These are ignored, except in
    \code{method ="gower"} which accepts \code{range.global} parameter of
    \code{\link{decostand}}. .}
}
\details{
  Jaccard (\code{"jaccard"}), Mountford (\code{"mountford"}),
  Raup--Crick (\code{"raup"}) and Binomial indices are discussed below.
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
    \tab \eqn{d_{jk}=\frac{1}{NZ} \sum_i
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
      x_{ij}^2/(\sum_i x_{ij})^2}{lambda[j] =
      sum(x[ij]^2)/(sum(x[ij])^2)}
    \cr
    \code{binomial}
    \tab \eqn{d_{jk} = \sum_i [x_{ij} \log (\frac{x_{ij}}{n_i}) + x_{ik} \log
      (\frac{x_{ik}}{n_i}) - n_i \log(\frac{1}{2})]/n_i}{d[jk] =
      sum(x[ij]*log(x[ij]/n[i]) + x[ik]*log(x[ik]/n[i]) -
      n[i]*log(1/2))/n[i]}
    \cr
    \tab where \eqn{n_i = x_{ij} + x_{ik}}{n[i] = x[ij] + x[ik]}
  }

  Jaccard index is computed as \eqn{2B/(1+B)}, where \eqn{B} is
  Bray--Curtis dissimilarity.

  Binomial index is derived from Binomial deviance under null hypothesis
  that the two compared communities are equal. It should be able to
  handle variable sample sizes. The index does not have a fixed upper
  limit, but can vary among sites with no shared species. For further
  discussion, see Anderson & Millar (2004).
  
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
  literature: indeed Mountford (1962) suggested an approximation to be
  used as starting
  value in iterations, but the proper index is defined as the root of
  the equation
  above. The function \code{vegdist} solves \eqn{M} with the Newton
  method. Please note that if either \eqn{a} or \eqn{b} are equal to
  \eqn{j}, one of the communities could be a subset of other, and the
  dissimilarity is \eqn{0} meaning that non-identical objects may be
  regarded as similar and the index is non-metric. The Mountford index
  is in the range \eqn{0 \dots \log(2)}, but the dissimilarities are
  divided by \eqn{\log(2)} 
  so that the results will be in the conventional range \eqn{0 \dots 1}.

  Raup--Crick dissimilarity (\code{method = "raup"}) is a probabilistic
  index based on presensec/absence data.  It is defined as \eqn{1 - prob(j)},
  or based on the probability of observing at least \eqn{j} 
  species in shared in compared communities.  Legendre & Legendre (1978)
  suggest
  using simulations to assess the probability, but the current function
  uses analytic result from hypergeometric distribution
  (\code{\link{phyper}}) instead.  This probability (and the index) is
  dependent on the number of species missing in both sites, and adding
  all-zero species to the data or removing missing species from the data
  will influence the index.  The probability (and the index) may be
  almost zero or almost one for a wide range of parameter values.  The
  index is nonmetric: two
  communities with no shared species may have a dissimilarity slightly
  below one, and two identical communities may have dissimilarity
  slightly above zero.
  
  Morisita index can be used with genuine count data (integers) only. Its
  Horn--Morisita variant is able to handle any abundance data.

  Euclidean and Manhattan dissimilarities are not good in gradient
  separation without proper standardization but are still included for
  comparison and special needs.

  Bray--Curtis and Jaccard indices are rank-order similar, and some
  other indices become identical or rank-order similar after some 
  standardizations, especially with presence/absence transformation of
  equalizing site totals with \code{\link{decostand}}. Jaccard index is
  metric, and probably should be preferred instead of the default
  Bray-Curtis which is semimetric. 

  The naming conventions vary. The one adopted here is traditional
  rather than truthful to priority. The function finds either
  quantitative or binary variants of the indices under the same name,
  which correctly may refer only to one of these alternatives For
  instance, the Bray
  index is known also as Steinhaus, Czekanowski and Sørensen index.
  The quantitive version of Jaccard should probably called
  Ruzicka index (but spelled with characters that cannot be shown here).
  The abbreviation \code{"horn"} for the Horn--Morisita index is
  misleading, since there is a separate Horn index. The abbreviation
  will be changed if that index is implemented in \code{vegan}. 
}
\value{
  Should provide a drop-in replacement for \code{\link{dist}} and
  return a distance object of the same type. 
}
\references{
  Anderson, M.J. and Millar, R.B. (2004). Spatial variation and effects
  of habitat on temperate reef fish assemblages in northeastern New
  Zealand.  \emph{Journal of Experimental Marine Biology and Ecology}
  305, 191--221.
  
  Faith, D. P, Minchin, P. R. and Belbin, L. (1987).
  Compositional dissimilarity as a robust measure of ecological
  distance. \emph{Vegetatio} 69, 57--68.

  Krebs, C. J. (1999). \emph{Ecological Methodology.} Addison Wesley Longman.

  Legendre, P, & Legendre, L. (1998) \emph{Numerical Ecology}. 2nd English
  Edition. Elsevier.

  
  Mountford, M. D. (1962). An index of similarity and its application to
  classification problems. In: P.W.Murphy (ed.),
  \emph{Progress in Soil Zoology}, 43--50. Butterworths.

  Wolda, H. (1981). Similarity indices, sample size and
  diversity. \emph{Oecologia} 50, 296--302.
}

\author{ Jari Oksanen, with contributions from Tyler Smith (Gower index)
  and Michael Bedward (Raup--Crick index). }

\note{The  function is an alternative to \code{\link{dist}} adding
  some ecologically meaningful indices.  Both methods should produce
  similar types of objects which can be interchanged in any method
  accepting either.  Manhattan and Euclidean dissimilarities should be
  identical in both methods. Canberra index is divided by the
  number of variables in \code{vegdist}, but not in \code{\link{dist}}.
  So these differ by a constant multiplier, and the alternative in
  \code{vegdist} is in range (0,1).  Function \code{\link[cluster]{daisy}}
  (package \pkg{cluster}) provides alternative implentation of Gower
  index for mixed data of numeric and class variables (but it works for
  mixed variables only).

  Most dissimilarity indices in \code{vegdist} are designed for
  community data, and they will give misleading values if there are
  negative data entries.  The results may also be misleading or
  \code{NA} or \code{NaN} if there are empty sites.  In principle, you
  cannot study species compostion without species and you should remove
  empty sites from community data.
}

\seealso{ \code{\link{decostand}}, \code{\link{dist}},
  \code{\link{rankindex}}, \code{\link[MASS]{isoMDS}},
  \code{\link{stepacross}}, \code{\link[cluster]{daisy}},
  \code{\link[labdsv]{dsvdis}}.  }

\examples{
data(varespec)
vare.dist <- vegdist(varespec)
# Orlóci's Chord distance: range 0 .. sqrt(2)
vare.dist <- vegdist(decostand(varespec, "norm"), "euclidean")
}
\keyword{ multivariate }
