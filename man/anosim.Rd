\name{anosim}
\alias{anosim}
\alias{print.anosim}
\alias{summary.anosim}
\alias{plot.anosim}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Analysis of Similarities }
\description{
  Analysis of similarities (ANOSIM) provides a way to test statistically
  whether there is a significant difference between two or more groups
  of sampling units.
}
\usage{
anosim(dis, grouping, permutations=1000, strata)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{dis}{Dissimilarity matrix.}
  \item{grouping}{Factor for grouping observations.}
  \item{permutations}{Number of permutation to assess the significance
    of the ANOSIM statistic. }
  \item{strata}{An integer vector or factor specifying the strata for
    permutation. If supplied, observations are permuted only within the
    specified strata.}
}
\details{
  Analysis of similarities (ANOSIM) provides a way to test statistically
  whether there is a significant difference between two or more groups
  of sampling units.  Function \code{anosim} operates directly on a
  dissimilarity matrix.  A suitable dissimilarity matrix is produced by
  functions \code{\link[mva]{dist}} or \code{\link{vegdist}}.  The
  method is philosophically allied with NMDS ordination
  (\code{\link[MASS]{isoMDS}}), in that it uses only the rank order of
  dissimilarity values.

  If two groups of sampling units are really different in their species
  composition, then compositional dissimilarities between the groups
  ought to be greater than those within the groups.  The \code{anosim}
  statistic \eqn{R} is based on the difference of mean ranks between
  groups (\eqn{r_B}) and within groups (\eqn{r_W}):

  \deqn{R = (r_B - r_W)/(N/(N-1)/4)}

  The divisor is chosen so that \eqn{R} will be in the interval
  \eqn{-1 \dots +1}, value \eqn{0} indicating completely random
  grouping.

  The statistical significance of observed \eqn{R} is assessed by
  permuting the grouping vector to obtain the empirical
  distribution of \eqn{R} under null-model.

  The function has \code{summary} and \code{plot} methods.  These both
  show valuable information to assess the validity of the method:  The
  function assumes that all ranked dissimilarities within groups 
  have about equal median and range.  The \code{plot} method uses
  \code{\link{boxplot}} with options \code{notch=TRUE} and
  \code{varwidth=TRUE}. 
}
\value{
  The function returs a list of class \code{anosim} with following items:
  \item{Call }{Function call.}
  \item{statistic}{The value of ANOSIM statistic \eqn{R}}
  \item{signif}{Significance from permutation.}
  \item{perm}{Permutation values of \eqn{R}}
  \item{class.vec}{Factor with value \code{Between} for dissimilarities
    between classes and class name for corresponding dissimilarity
    within class.}
  \item{dis.rank}{Rank of dissimilarity entry.}
}
\references{
  Clarke, K. R. (1993). Non-parametric multivariate analysis of changes
  in community structure. \emph{Australian Journal of Ecology} 18, 117-143.
}
\author{Jari Oksanen, with the help from Peter R. Minchin.}
\note{
  I don't quite trust this method.  Somebody should study its
  performance carefully.  The function returns a lot information just to
  ease further scrutiny.

}

\seealso{\code{\link[mva]{dist}} and \code{\link{vegdist}} for obtaining
  dissimilarities, and \code{\link{rank}} for ranking real values.  For
  comparing dissimilarities against continuous variables, see
  \code{\link{mantel}}.  }

\examples{
## There are no factors yet in example data set, so let's make one (this
## is real though).
reindeer <- c(rep("Grazed", 16), rep("Ungrazed", 8))
data(varespec)
vare.dist <- vegdist(varespec)
vare.ano <- anosim(vare.dist, reindeer)
summary(vare.ano)
plot(vare.ano)
}
\keyword{multivariate }
\keyword{ nonparametric }
\keyword{ htest }
