\name{vectorfit}
\alias{vectorfit}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Fits an Environmental Vector to an Ordination }
\description{
  The function fits environmental vectors onto an
  ordination, so that projection of points onto vectors have
  maximum correlations with corresponding environmental variables.
}
\usage{
vectorfit(X, P)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{X}{ Ordination configuration.}
  \item{P}{ Matrix or vector of environmental variable(s). }
}
\details{
  The function finds directions in the ordination space towards which
  the environmental vectors change most rapidly and to which they have
  maximal correlations with the ordination configuration.
}
\value{
  Returns a matrix where rows correspond to environmental variables and
  first \eqn{k} columns labeled \code{head1 \ldots headk} give the
  endpoints of environmental vectors (or their direction cosines) and
  the last row gives the correlation coefficient to the fitted vector.
  The vectors are of unit length.  Further scaling is left for the
  user.

}
\references{ I found this in folklore\ldots I hope this is correct\ldots  }
\author{Jari Oksanen }

\seealso{\code{\link[MASS]{isoMDS}} }

\examples{
library(MASS)
library(mva)
data(varespec)
data(varechem)
vare.dist <- vegdist(wisconsin(varespec))
vare.mds <- isoMDS(vare.dist)
vare.points <- postMDS(vare.mds$points, vare.dist)
vare.fit <- vectorfit(vare.points, varechem)
## Scale vectors by correlation coefficient
vare.wfit <- vare.fit[,1:2]*sqrt(vare.fit[,"r"])
\dontrun{eqscplot(vare.points, pch="+", tol=0.1)}
\dontrun{arrows(0,0, vare.wfit[,1], vare.wfit[,2], len=0.1, col="blue")}
\dontrun{text(1.05*vare.wfit, rownames(vare.wfit))}
}
\keyword{multivariate }%-- one or more ...


