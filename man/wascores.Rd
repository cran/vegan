\name{wascores}
\alias{wascores}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Weighted Averages Scores for Species }
\description{
  Computes Weighted Averages scores of species for ordination
  configuration or for environmental variables.
}
\usage{
wascores(x, w, expand=FALSE)
} 
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{Environmental variables or ordination scores.}
  \item{w}{Weights: species abundances.}
  \item{expand}{ Not yet implemented. }
}
\details{
  Computes weighted averages.
}
\value{
  Returns a matrix where species define rows and environmental variables
  define columns.
}

\author{ Jari Oksanen }

\seealso{ \code{\link[MASS]{isoMDS}} }

\examples{
library(MASS)
library(mva)
data(varespec)
data(varechem)
vare.dist <- vegdist(wisconsin(varespec))
vare.mds <- isoMDS(vare.dist)
vare.points <- postMDS(vare.mds$points, vare.dist)
vare.wa <- wascores(vare.points, varespec)
\dontrun{eqscplot(vare.points, pch="+")}
\dontrun{text(vare.wa, rownames(vare.wa), cex=0.8, col="blue")}
## Omit rare species (frequency <= 4)
freq <- apply(varespec>0, 2, sum)
\dontrun{eqscplot(vare.points, pch="+")}
\dontrun{text(vare.wa[freq > 4,], rownames(vare.wa)[freq > 4], cex=0.8, col="blue")}
}
\keyword{ multivariate }%-- one or more ...
\keyword{ univar }
