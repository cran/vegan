\name{diversity}
\alias{diversity}

\title{ Ecological Diversity Indices }
\description{
Shannon and Simpson diversity indices for community ecologists.
}
\usage{
diversity(x, index="shannon", MARGIN=1, base=exp(1))
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{Community data matrix}
  \item{index}{Diversity index, one of \code{shannon}, \code{simpson} or
  \code{invsimpson}.}
  \item{MARGIN}{Margin for which the index is computed. }
  \item{base}{ The logarithm \code{base} used in \code{shannon}  }
}
\details{
  Shannon or Shannon--Weaver (or Shannon--Wiener) index is defined as
  \eqn{H' = -\sum_i p_i \log_{b} p_i}{H = -sum p_i log(b) p_i}, where
  \eqn{p_i} is the proportional abundance of species \eqn{i} and \eqn{b}
  is the base of the logarithm.  It is most popular to use natural
  logarithms, but some argue for base \eqn{b = 2} (which makes sense, but no
  difference).

  Both variants of Simpson's index are based on \eqn{S = \sum p_i^2}{S =
    sum p_i^2}. Choice \code{simpson} returns \eqn{1-S} and
  \code{invsimpson} returns \eqn{1/S}.
}
\value{
  Vector of diversity indices.
}

\author{ Jari Oksanen }


\examples{
data(varespec)
H <- diversity(varespec)
## Species richness (S) and Pielou's evennes (J):
S <- apply(varespec>0, 1, sum)
J <- H/log(S)
}
\keyword{ univar }%-- one or more ...

