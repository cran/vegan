\name{decostand}
\alias{decostand}
\alias{wisconsin}

\title{Standardizaton Methods for Community Ecology}
\description{
The function provides some popular (and effective) standardization
methods for community ecologists.
}
\usage{
decostand(x, method, MARGIN)
wisconsin(x)
}

\arguments{
  \item{x}{Community data matrix.}
  \item{method}{Standardization method.}
  \item{MARGIN}{Margin, if default is not acceptable.}
}
\details{
  The function offers following standardization methods for community
  data:
  \itemize{
    \item \code{total}: divide by margin total (default \code{MARGIN=1}).
    \item \code{max}: divide by margin maximum (default \code{MARGIN=2}).
    \item \code{freq}: divide by margin maximum and multiply by number of
    non-zero items, so that the average of non-zero entries is one (default
    \code{MARGIN=2}).
    \item \code{normalize}: make margin sum of squares equal to unity (default
    \code{MARGIN=1}).
    \item \code{range}: standardize values into range 0 \dots 1 (default
    \code{MARGIN=2}).
    \item \code{standardize}: scale into zero mean and unit variance
    (default \code{MARGIN=2}).
    \item \code{pa}: scale into presence/absence scale (0/1).
  }
  Standardization, as contrasted to transformation, means that the
  entries are transformed relative to other entries.

  All methods have a default margin.  \code{MARGIN=1} means rows (sites
  in a
  normal data set) and \code{MARGIN=2} means columns (species in a normal
  data set).

  Command \code{wisconsin} is a shortcut to common Wisconsin double
  standardization where species (\code{MARGIN=2}) are first standardized
  by maxima (\code{max}) and then sites (\code{MARGIN=1}) by
  site totals (\code{tot}). 
}
\value{
  Returns the standardized matrix.
}
\author{Jari Oksanen}
\note{Common transformations can be made with standard \R functions.}


\examples{
data(varespec)
sptrans <- decostand(varespec, "max")
apply(sptrans, 2, max)
sptrans <- wisconsin(varespec)
}
\keyword{ multivariate}%-- one or more ...
\keyword{ manip }

