\name{diversity}
\alias{diversity}
\alias{rarefy}
\alias{fisher.alpha}

\title{ Ecological Diversity Indices and Rarefaction Species Richness }

\description{ Shannon, Simpson and Fisher diversity indices and rarefied
species richness for community ecologists.  }

\usage{
diversity(x, index = "shannon", MARGIN = 1, base = exp(1))
rarefy(x, sample, MARGIN = 1)
fisher.alpha(x, MARGIN = 1, se = FALSE, ...) 
}

\arguments{
  \item{x}{Community data matrix.}
  \item{index}{Diversity index, one of \code{shannon}, \code{simpson} or
  \code{invsimpson}.}
  \item{MARGIN}{Margin for which the index is computed. }
  \item{base}{ The logarithm \code{base} used in \code{shannon}.}
  \item{sample}{Subsample size for rarefying community.}
  \item{se}{Estimate standard errors.}
  \item{...}{Parameters passed to \code{\link{nlm}}}
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
  species. Please note that rarefaction can be done only
  with genuine
  counts of individuals: the current function will silently truncate
  abundances to integers and give wrong results. The function
  \code{rarefy} is based on Hurlbert's (1971) formulation.

  Function \code{fisher.alpha} estimates the \eqn{\alpha} parameter of
  Fisher's logarithmic series where the expected
  number of species \eqn{f} with \eqn{n} observed individuals is
  \eqn{f_n = \alpha x^n / n} (Fisher et al. 1943). The estimation
  follows Kempton & Taylor (1974) and uses function
  \code{\link{nlm}}. The estimation is possible only for genuine
  counts of individuals. The function can optionally return standard
  errors of \eqn{\alpha}.  These should be regarded only as rough
  indicators of the accuracy: the confidence limits of \eqn{\alpha} are
  strongly
  non-symmetric and standard errors cannot be used in Normal inference.
  
  Better stories can be told about Simpson's index than about
  Shannon's index, and still more grandiose stories about
  rarefaction (Hurlbert 1971).  However, these indices are all very
  closely related (Hill 1973), and there is no reason to despise one more than
  others (but if you are a graduate student, don't drag me in, but obey
  your Professor's orders). In particular, exponent of the Shannon
  index is linearly related to inverse Simpson (Hill 1973) although the
  former may be more sensitive to rare species. Moreover, inverse
  Simpson is asymptotically equal to rarefied species richness in sample
  of two indivividuals, and Fisher's \eqn{\alpha} is very similar to
  inverse Simpson.
}

\value{
  Vector of diversity indices or rarefied species richness values. With
  option \code{se = TRUE}, function \code{fisher.alpha} returns a data
  frame with items for \eqn{\alpha} (\code{alpha}), its approximate
  standard errors (\code{se}), residual degrees of freedom
  (\code{df.residual}), and the \code{code} returned by
  \code{\link{nlm}} on the success of estimation. }
}

\references{
  Fisher, R.A., Corbet, A.S. & Williams, C.B. (1943). The relation
  between the number of species and the number of individuals in a
  random sample of animal population. \emph{Journal of Animal Ecology}
  12: 42-58.
  
  Hill, M.O. (1973). Diversity and evenness: a unifying notation and its
  consequences. \emph{Ecology} 54: 427-473.
  
  Hurlbert, S.H. (1971). The nonconcept of species diversity: a critique
  and alternative parameters. \emph{Ecological Monographs} 54: 187-211.

  Kempton, R.A. & Taylor, L.R. (1974). Log-series and log-normal
  parameters as diversity discriminators for Lepidoptera. \emph{Journal
    of Animal Ecology} 43: 381-399.  
  }

\author{ Jari Oksanen and Bob O'Hara. }


\examples{
data(varespec)
H <- diversity(varespec)
## Species richness (S) and Pielou's evenness (J):
S <- colSums(varespec>0)
J <- H/log(S)
## Other indices cannot be demonstrated because there are not yet
## count data sets in vegan.
}
\keyword{ univar }%-- one or more ...

