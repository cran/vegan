\name{wascores}
\alias{wascores}

\title{ Weighted Averages Scores for Species }
\description{
  Computes Weighted Averages scores of species for ordination
  configuration or for environmental variables.
}
\usage{
wascores(x, w, expand=FALSE)
} 

\arguments{
  \item{x}{Environmental variables or ordination scores.}
  \item{w}{Weights: species abundances.}
  \item{expand}{Expand weighted averages so that they have the same
    weighted variance as the corresponding environmental variables.  }
}
\details{
  Computes weighted averages. Weighted averages `shrink': they cannot be
  more extreme than values usd for calculating the averages. With
  \code{expand = TRUE}, the function `dehsrinks' the weighted averages
  by making their weighted variance equal to the weighted variance of
  the corresponding environmental variable.
}
\value{
  Returns a matrix where species define rows and ordination axes or
  environmental variables define columns.
}

\author{ Jari Oksanen }

\seealso{ \code{\link[MASS]{isoMDS}} }

\examples{
data(varespec)
data(varechem)
library(MASS)  ## isoMDS
library(mva)   ## cmdscale to start isoMDS
vare.dist <- vegdist(wisconsin(varespec))
vare.mds <- isoMDS(vare.dist)
vare.points <- postMDS(vare.mds$points, vare.dist)
vare.wa <- wascores(vare.points, varespec)
plot(scores(vare.points), pch="+", asp=1)
text(vare.wa, rownames(vare.wa), cex=0.8, col="blue")
## Omit rare species (frequency <= 4)
freq <- apply(varespec>0, 2, sum)
plot(scores(vare.points), pch="+", asp=1)
text(vare.wa[freq > 4,], rownames(vare.wa)[freq > 4],cex=0.8,col="blue")
## Works for environmental variables, too.
wascores(varechem, varespec)
}

}
\keyword{ multivariate }
\keyword{ univar }



