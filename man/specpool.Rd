\name{specpool}
\alias{specpool}
\alias{specpool2vect}
\title{ Extrapolated Species Richness in a Species Pool}
\description{
  The function estimates the extrapolated species richness in a species
  pool, or the number of unobserved species.
}
\usage{
specpool(x, pool)
specpool2vect(X, index = c("Jack.1","Jack.2", "Chao", "Boot", "Species"))
}

\arguments{
  \item{x}{Data frame or matrix with species data.}
  \item{pool}{A vector giving a classification for pooling the sites in
    the species data. If missing, all sites are pooled together.}
  \item{X}{A \code{specpool} result object.}
  \item{index}{The selected index of extrapolated richness.}
}
\details{
  Many species will always remain unseen or undetected in a collection
  of sample plots.  The function uses some popular ways of estimating
  the number of these unseen species and adding them to the observed
  species richness (Palmer 1990, Colwell & Coddington 1994).

  In the following, \eqn{S_P} is the extrapolated richness in a pool,
  \eqn{S_0} is the observed number of species in the
  collection, \eqn{a_1}{a1} and \eqn{a_2}{a2} are the number of species
  occurring only in one or only in two sites in the collection, \eqn{p_i}
  is the frequency of species \eqn{i}, and \eqn{N} is the number of
  sites in the collection.  The variants of extrapolated richness are:
  \tabular{ll}{
    Chao
    \tab \eqn{S_P = S_0 + \frac{a_1^2}{2 a_2}}{S_P = S_0 + a1/2/a2}
    \cr
    First order jackknife
    \tab \eqn{S_P = S_0 + a_1 \frac{N-1}{N}}{S_P = S_0 + a1*(N-1)/N}
    \cr
    Second order jackknife
    \tab \eqn{S_P = S_0 + a_1 \frac{2N - 3}{N} - a_2 \frac{(N-2)^2}{N
	(N-1)}}{S_P = S_0 + a1*(2*n-3)/n - a2*(n-2)^2/n/(n-1)}
    \cr
    Bootstrap
    \tab \eqn{S_P = S_0 + \sum_{i=1}^{S_0} (1 - p_i)^N}{S_P = S_0 + Sum
      (1-p_i)^N}
    }
 
}
\value{
  The function returns a data frame with entries for observed richness
  and each of the indices for each class in \code{pool} vector.  The
  utility function \code{specpool2vect} maps the pooled values into
  a vector giving the value of selected \code{index} for each original
  site. 
}
\references{
  Colwell, R.K. & Coddington, J.A. (1994). Estimating terrestrial
  biodiversity through
  extrapolation. \emph{Phil. Trans. Roy. Soc. London} B 345, 101--118.

  Palmer, M.W. (1990). The estimation of species richness by
  extrapolation. \emph{Ecology} 71, 1195--1198.


}
\author{Jari Oksanen }
\note{
  The functions are based on assumption that there is a species pool:
  The community is closed so that there is a fixed pool size \eqn{S_P}.
  Such cases may exist, although I have not seen them yet.  All indices
  are biased for open communities.

  An approximate ("traditional") variant is used for the Chao index.

  The function is still preliminary.  I may add variances, although
  these seem to be biased and confusing.

  See \url{http://viceroy.eeb.uconn.edu/EstimateS} for a more complete
  (and positive) discussion and alternative software for some platforms.
}
\seealso{\code{\link{veiledspec}}, \code{\link{diversity}}. }
\examples{
data(dune)
data(dune.env)
attach(dune.env)
pool <- specpool(dune, Management)
pool
op <- par(mfrow=c(1,2))
boxplot(specnumber(dune) ~ Management, col="hotpink", border="cyan3",
 notch=TRUE)
boxplot(specnumber(dune)/specpool2vect(pool) ~ Management, col="hotpink",
 border="cyan3", notch=TRUE)
par(op)
}
\keyword{ univar }

