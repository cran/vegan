\encoding{utf8}
\name{beals}
\alias{beals}

\title{Beals Smoothing of Community Data }
\description{
  Beals smoothing replaces each entry in the community data with a
  probability of target species occurring in that particular site, based
  on the joint occurrences of target species with the species that
  actually occur in the site.
}
\usage{
beals(x)
}
\arguments{
  \item{x}{Community data frame or matrix }
}
\details{
  The Beals smoothing is the estimated probability \eqn{p_{ij}}{p[ij]}
  that species \eqn{j} occurs in site \eqn{i}.  It is defined as
  \eqn{p_{ij} = \frac{1}{S_i} \sum_k \frac{N_{jk} I_{ik}}{N_k}}{p[ij]
    = 1/S[i] Sum(k) N[jk] I[ik] / N[k]}, where \eqn{S} is the
  number of species on a site, \eqn{N_{jk}}{N[jk]} is the number of
  joint occurrences of species \eqn{j} and \eqn{k}, and
  \eqn{N_k}{N[k]} is the number of occurences of species \eqn{k}, and
  \eqn{I} is the incidence (0 or 1) of species (this last term is
  usually omitted from the equation, but it is necessary).

  Beals smoothing was originally suggested as a method of data
  transformation to remove excessive zeros (Beals 1984, McCune
  1994).  However, it is not a suitable method for this purpose since it
  does not maintain the information on species presences:  A species may
  have a higher probability of occurrence in a site where it does not
  occur than in sites where it occurs.  Moreover, it regularizes data
  too strongly.  The method may be useful in identifying species that
  belong to the species pool (Ewald 2002) or to identify suitable
  unoccupied patches in metapopulation analysis
  (\enc{Münzbergová}{Munzbergova} & Herben 
  2004).  The function is provided here for the brave.
}
\value{
  The function returns a transformed data matrix.
}
\references{
  
Beals, E.W. (1984): Bray-Curtis-ordination: an effective strategy for
analysis of multivariate ecological data.  \emph{Adv. Ecol. Res.} 14: 1-55.
  
Ewald, J. 2002. A probabilistic approach to estimating species pools
from large compositional matrices. \emph{J. Veg. Sci.} 13: 191-198.

McCune, B. 1994. Improving community ordination with the Beals smoothing
function. \emph{Ecoscience} 1: 82-86.

\enc{Münzbergová}{Munzbergova}, Z. & Herben, T. 2004. Identification of
suitable unoccupied 
habitats in metapopulation studies using co-occurrence of species. \emph{Oikos}
105: 408-414.

}
\author{Jari Oksanen}
\note{
  The current function is modelled as closely as possible after Beals
  (1984) and McCune (1994).  It seems that
  \enc{Münzbergová}{Munzbergova} and Herben (2004) 
  use slightly different formulation.
}
\seealso{\code{\link{decostand}} for proper standardization methods,
  \code{\link{specpool}} for an attempt to assess the size of species
  pool. }
\examples{
data(dune)
x <- beals(dune)
## Smoothed values against presence or absence of species
pa <- decostand(dune, "pa")
boxplot(as.vector(x) ~ unlist(pa), xlab="Presence", ylab="Beals")
}
\keyword{ manip }
\keyword{ smooth }
