\name{diversity}
\alias{diversity}
\alias{rarefy}

\title{ Ecological Diversity Indices and Rarefaction Species Richness }
\description{
Shannon and Simpson diversity indices and rarefied species richness  for
community ecologists. 
}
\usage{
diversity(x, index = "shannon", MARGIN = 1, base = exp(1))
rarefy(x, sample, MARGIN = 1)
}

\arguments{
  \item{x}{Community data matrix.}
  \item{index}{Diversity index, one of \code{shannon}, \code{simpson} or
  \code{invsimpson}.}
  \item{MARGIN}{Margin for which the index is computed. }
  \item{base}{ The logarithm \code{base} used in \code{shannon}.}
  \item{sample}{Subsample size for rarefying community.}
}
\details{
  Shannon or Shannon--Weaver (or Shannon--Wiener) index is defined as
  \eqn{H' = -\sum_i p_i \log_{b} p_i}{H = -sum p_i log(b) p_i}, where
  \eqn{p_i} is the proportional abundance of species \eqn{i} and \eqn{b}
  is the base of the logarithm.  It is most popular to use natural
  logarithms, but some argue for base \eqn{b = 2} (which makes sense, but no
  real difference).

  Both variants of Simpson's index are based on \eqn{S = \sum p_i^2}{S =
    sum p_i^2}. Choice \code{simpson} returns \eqn{1-S} and
  \code{invsimpson} returns \eqn{1/S}.

  Function \code{rarefy} gives the expected species richness in random
  subsamples of size \code{sample} from the community.  The maximum
  permissible sample size is \eqn{N - \max(n_i)}, where \eqn{N} is the
  total number of individuals and \eqn{n_i} are the abundances of
  species on site \eqn{i}. Please note that rarefaction can be done only with real
  counts of individuals: the current function will silently truncate
  abundances to integers and give wrong results. The function
  \code{rarefy} is based on Hurlbert's (1971) formulation.

  Better histories can be told about Simpson's index than about
  Shannon's index, and still more grandiose stories about
  rarefaction (Hurlbert 1971).  However, these indices are all very
  closely related, and there is no reason to despise one more than
  others (but if you are a graduate student, don't drag me in, but obey
  your Professor's orders).
}

\value{
  Vector of diversity indices or rarefied species richness values.}
}

\references{
  Hurlbert, S.H. (1971). The nonconcept of species diversity: a critique
  and alternative parameters. \emph{Ecological Monographs} 54: 187-211.
  }

\author{ Jari Oksanen }


\examples{
data(varespec)
H <- diversity(varespec)
## Species richness (S) and Pielou's evenness (J):
S <- apply(varespec>0, 1, sum)
J <- H/log(S)
}
\keyword{ univar }%-- one or more ...

