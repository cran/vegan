\name{bioenv}
\alias{bioenv}
\alias{bioenv.default}
\alias{bioenv.formula}
\alias{print.bioenv}
\alias{summary.bioenv}
\alias{print.summary.bioenv}
\alias{ripley.subsets}
\alias{ripley.subs}

\title{Best Subset of Environmental Variables with
  Maximum (Rank) Correlation with Community Dissimilarities }
\description{
  Function finds the best subset of environmental variables, so that
  the Euclidean distances of scaled environmental variables have the
  maximum (rank) correlation with community dissimilarities.  
}
\usage{
\method{bioenv}{default}(comm, env, method = "spearman", index = "bray",
       upto = ncol(env), ...)
\method{bioenv}{formula}(formula, data, ...)
}

\arguments{
  \item{comm}{Community data frame. }
  \item{env}{Data frame of continuous environmental variables. }
  \item{method}{The correlation method used in \code{\link{cor.test}}.}
  \item{index}{The dissimilarity index used for community data in
    \code{\link{vegdist}}. }
  \item{upto}{Maximum number of parameters in studied subsets.}
  \item{formula, data}{Model \code{\link{formula}} and data.}
  \item{...}{Other parameters passed to function.}
}
\details{
  The function calculates a community dissimilarity matrix using
  \code{\link{vegdist}}.  Then it selects all possible subsets of
  environmental variables, \code{\link{scale}}s the variables, and
  calculates Euclidean distances for this subset using
  \code{\link{dist}}.  Then it finds the correlation between community
  dissimilarities and environmental distances, and for each size of
  subsets, saves the best result. 
  There are \eqn{2^p-1} subsets of \eqn{p} variables, and exhaustive
  search may take a very, very, very long time (parameter \code{upto} offers a
  partial relief). 

  The function can be called with a model \code{\link{formula}} where
  the LHS is the data matrix and RHS lists the environmental variables.
  The formula interface is practical in selecting or transforming
  environmental variables.

  Clarke & Ainsworth (1993) suggested this method to be used for
  selecting the best subset of environmental variables in interpreting
  results of nonmetric multidimensional scaling (NMDS). They recommended a
  parallel display of NMDS of community dissimilarities and NMDS of
  Euclidean distances from the best subset of scaled environmental
  variables.  They warned against the use of Procrustes analysis, but
  to me this looks like a good way of comparing these two ordinations.

  Clarke & Ainsworth wrote a computer program BIO-ENV giving the name to
  the current function. Presumably BIO-ENV
  was later incorporated in Clarke's PRIMER software (available for
  Windows).  In addition, Clarke & Ainsworth suggested a novel method of
  rank correlation which is not available in the current function.
}
\value{
  The function returns an object of class \code{bioenv} with a
  \code{summary} method.
}
\references{
  Clarke, K. R & Ainsworth, M. 1993. A method of linking multivariate
  community structure to environmental variables. \emph{Marine Ecology
    Progress Series}, 92, 205--219.
}
\author{ Jari Oksanen. The code for selecting all possible subsets was
  posted to the R mailing list by Prof. B. D. Ripley in 1999. }
\note{
  Function \code{\link{cor.test}} will give harmless warnings on ties.
}

\seealso{\code{\link{vegdist}},
  \code{\link{dist}}, \code{\link{cor.test}} for underlying routines,
  \code{\link[MASS]{isoMDS}} for ordination, \code{\link{procrustes}}
  for Procrustes analysis, \code{\link{protest}} for an alternative, and
  \code{\link{rankindex}} for studying alternatives to the default
  Bray-Curtis index.}

\examples{
# The method is very slow for large number of possible subsets.
# Therefore only 6 variables in this example.
data(varespec)
data(varechem)
sol <- bioenv(wisconsin(varespec) ~ log(N) + P + K + Ca + pH + Al, varechem)
sol
summary(sol)
}
\keyword{ multivariate }

