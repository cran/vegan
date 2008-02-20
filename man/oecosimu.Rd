\encoding{UTF-8}
\name{oecosimu}
\alias{oecosimu}
\alias{nestedchecker}
\alias{nestedn0}
\alias{nesteddisc}
\alias{commsimulator}
\alias{print.oecosimu}
\alias{print.nestedchecker}
\alias{print.nestedn0}
\alias{print.nesteddisc}

\title{ Nestedness and Null Models for Islands or Patches }

\description{
  Patches or local communities are regarded as nested if they all could
  be subsets of the same community. In general, species poor communities
  should be subsets of species rich communities, and rare species should
  only occur in species rich communities. Null models generate random
  communities with different criteria to study the significance of
  nestedness.
}

\usage{
oecosimu(comm, nestfun, method, nsimul = 99, burnin = 0, thin = 1, ...)
nestedchecker(comm)
nestedn0(comm)
nesteddisc(comm)
commsimulator(x, method, thin=1)
}

\arguments{
  \item{comm}{Community data. }
  \item{nestfun}{Function to analyse nestedness. Some functions are
  provided in \pkg{vegan}, but any function can be used if it accepts the
  community as the first argument, and returns the result in item
  \code{statistic}. See Examples for defining your own functions.}
  \item{method}{Null model method. See details.}
  \item{nsimul}{Number of simulated null communities.}
  \item{burnin}{Number of null communities discarded before proper
  analysis in sequential methods \code{"swap"} and \code{"tswap"}.}
  \item{thin}{Number of discarded null communities between two
  evaluations of nestedness statistic in sequential methods
  \code{"swap"} and \code{"tswap"}.}
  \item{x}{Matrix for \code{commsimulator}.}
  \item{\dots}{Other arguments to functions.}
}

\details{
  Function \code{oecosimu} is a wrapper that evaluates a nestedness
  statistic using function given by \code{nestfun}, and then simulates a
  series of null models using \code{commsimulator}, and evaluates the
  nestedness statistic on these null models. The \pkg{vegan} packages
  contains some nestedness functions that are described below, and the
  generation of null models is described towards the end of this
  section.

  Function \code{netstedchecker} gives the number of checkerboard
  units, or 2x2 submatrices where both species occur once but on
  different sites (Stone & Roberts 1990).  Function \code{nestedn0}
  implements nestedness measure N0 which is the number of absences
  from the sites which are richer than the most pauperate site species
  occurs (Patterson & Atmar 1986).  Function \code{nesteddisc}
  implements discrepancy index which is the number of ones that should
  be shifted to fill a row with ones in a table arranged by species
  frequencies (Brualdi & Sanderson 1999).

  In addition to these functions provided in \pkg{vegan}, any function
  can be used that takes \code{comm} as the first argument, and
  returns the nestedness index in item \code{statistic}. Function
  \code{\link{chisq.test}} is such a function, and examples show how
  to do this in general.  If you write a function that may be useful
  to others, please consider submitting it to \pkg{vegan} for others
  to enjoy (you will be credited as an author of that function).

  Function \code{commsimulator} implements null models for community
  composition. The implemented models are \code{r00} which maintains the
  number of presences but fills these anywhere so that neither species
  (column) nor site (row) totals are preserved. Methods \code{r0},
  \code{r1} and \code{r2} maintain the site (row) frequencies. Method \code{r0}
  fills presences anywhere on the row with no respect to species (column)
  frequencies, \code{r1} uses column marginal 
  frequencies as probabilities, and \code{r2} uses squared column
  sums. Methods \code{r1} and \code{r2} try to simulate original species
  frequencies, but they are not strictly constrained. All these methods
  are reviewed by Wright et al. (1998). Method \code{c0} maintains
  species frequencies, but does not honour site (row) frequencies (Jonsson
  2001).

  The other methods maintain both row and column frequencies.
  Methods \code{swap} and \code{tswap} inspect random 2x2 submatrices
  and if they are checkerboard units, the order of columns is
  swapped. This changes the matrix structure, but does not influence
  marginal sums (Gotelli & Entsminger
  2003). Method \code{swap} inspects submatrices so long that a swap
  can be done. \enc{Miklós}{Miklos} & Podani (2004) suggest that this may lead into
  biased sequence, since some columns or rows may be more easily
  swapped, and they suggest trying a fixed number of times and
  doing zero to many swaps at one step. This method is implemented by
  method \code{tswap} or trial swap. Function \code{commsimulator} makes
  only one trial swap in time (which probably does nothing),
  but \code{oecosimu} estimates how many
  submatrices are expected before finding a swappable checkerboard,
  and uses that ratio to thin the results, so that on average one swap
  will be found per step of \code{tswap}.  However, the checkerboard
  frequency probably changes during swaps, but this is not taken into
  account in estimating the \code{thin}.  Both swap methods are sequential,
  and the result of the swap is used as the input of the next swap. One
  swap still changes the matrix only little, and it may be useful to
  thin the results so that the statistic is only evaluated after
  \code{thin} steps. 

  Method \code{backtracking}
  implements a filling method with constraints both for row and column
  frequencies (Gotelli & Entsminger 2001). The matrix is first filled
  randomly using row and column frequencies as probabilities. Typically
  row and column sums are reached before all incidences are filled in.
  After that begins \dQuote{backtracking}, where some of the
  points are removed, and then filling is started again, and this
  backtracking is done so may times that all incidences will be filled
  into matrix. Method
  \code{quasiswap} (\enc{Miklós}{Miklos} & Podani 2004) implements a method where
  matrix is first filled 
  honouring row and column totals, but with integers that may be larger than
  one. Then the method inspects random 2x2 matrices and performs a
  quasiswap on them. Quasiswap is similar to ordinary swap, but it also
  can reduce numbers above one to ones maintaining marginal
  totals. The \code{quasiswap} method is not sequential, but it produces
  a random incidence matrix with given marginal totals. 
}

\value{
  Function \code{oecosimu} returns the result of \code{nestfun}
  with one added component called \code{oecosimu}. The \code{oecosimu}
  component contains the simulated values of the statistic (item
  \code{simulated}), the name of the \code{method}, two-sided \eqn{P}
  value and z-value of the statistic. The result returned by
  nestedness function must contain item called \code{statistic}, but
  the other components differ among functions. The
  \code{commsimulator} returns a null model matrix or a swap of the
  input matrix.  }

\references{
  Brualdi, R.A. & Sanderson, J.G. (1999). Nested species subsets, gaps,
  and discrepancy. \emph{Oecologia} 119, 256--264.

  Gotelli, N.J. & Entsminger, N.J. (2001). Swap and fill algorithms in
  null model analyis: rethinking the knight's tour. \emph{Oecologia}
  129, 281--291.

  Gotelli, N.J. & Entsminger, N.J. (2003). Swap algorithms in null model
  analysis. \emph{Ecology} 84, 532--535.

  Jonsson, B.G. (2001) A null model for randomization tests of
  nestedness in species assemblages. \emph{Oecologia} 127, 309--313.

  \enc{Miklós}{Miklos}, I. & Podani, J. (2004). Randomization of presence-absence
  matrices: comments and new algorithms. \emph{Ecology} 85, 86--92.
  
  Patterson, B.D. & Atmar, W. (1986). Nested subsets and the structure
  of insular mammalian faunas and archipelagos. \emph{Biol. J. Linnean
    Soc.} 28, 65--82.

  Stone, L. & Roberts, A. (1990). The checkerboard score and species
  distributions. \emph{Oecologia} 85, 74--79.

  Wright, D.H., Patterson, B.D., Mikkelson, G.M., Cutler, A. & Atmar,
  W. (1998). A comparative analysis of nested subset patterns of species
  composition. \emph{Oecologia} 113, 1--20.
  }
\author{ Jari Oksanen }
\note{
  Functions \code{commsimulator} and \code{oecosimu} do not have
  default \code{nestfun} nor default \code{method}, because there is
  no clear natural choice. If you use these methods, you must be able
  to choose your own strategy. The choice of nestedness index is
  difficult because the functions seem to imply very different
  concepts of structure and randomness. The choice of swapping method
  is also problematic. Method \code{r00} has some heuristic value of
  being really random. However, it produces null models which are
  different from observed communities in most respects, and a
  \dQuote{significant} result may simply mean that not all species are
  equally common (\code{r0} is similar with this respect). It is also
  difficult to find justification for \code{r2}. The methods
  maintaining both row and column totals only study the community
  relations, but they can be very slow. Moreover, they regard marginal
  totals as constraints instead of results of occurrence patterns. You
  should evaluate timings in small trials (one cycle) before launching
  an extensive simulation. One swap is fast, but it changes data only
  little, and you may need long \code{burnin} and strong
  \code{thin}ning in large matrices. You should plot the simulated
  values to see that they are more or less stationary and there is no
  trend. Method \code{quasiswap} is implemented
  in plain R, and it is very slow, and it slows down very strongly
  with big matrices. In general, \code{backtrack} is faster and less
  sensitive to matrix size, but it also can be very slow.

  If you wonder about the name of \code{oecosimu}, look at journal
names in the References.  }

\seealso{ \code{\link{r2dtable}} generates table with given marginals but
  with entries above one.  Function \code{\link[labdsv]{rndtaxa}}
  (\pkg{labdsv} package) randomizes a community table.}
\examples{
data(sipoo)
nestedchecker(sipoo)
## Use the first eigenvalue of correspondence analysis as an index
## of structure: a model for making your own functions.
## This is a minimal structure; fancier functions give fancier results
caeval <- function(x) list(statistic = decorana(x, ira=1)$evals[1])
out <- oecosimu(sipoo, caeval, "swap", burnin=100, thin=5)
out
## Inspect the swap sequence
plot(out$oecosimu$simulated, type="l")
}
\keyword{ multivariate }

