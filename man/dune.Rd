\name{dune}
\alias{dune}
\alias{dune.env}
\non_function{}
\title{Vegetation and Environment in Dutch Dune Meadows. }
\usage{
  data(dune)
  data(dune.env)
}
\description{
The dune meadow vegetation data \code{dune} has cover class values of 30
species on 20 sites. The corresponding environmental data frame
\code{dune.env} has following entries:
}
\format{
  \describe{
    \item{A1}{a numeric vector of thickness of A1 horizon.}
    \item{Moisture}{an ordered factor with levels}
    \item{Moisture}{\code{1} < \code{2} < \code{4} < \code{5}}
    \item{Management}{a factor with levels}
    \item{Management}{\code{BF}: Biological Farming  }
    \item{Management}{\code{HF}: Hobby Farming }
    \item{Management}{\code{NM}: Nature Conservation Management }
    \item{Management}{\code{SF}: Standard Farming }
    \item{Use}{an ordered factor of landuse with levels}
    \item{Use}{\code{Hayfield} < \code{Haypastu} < \code{Pasture}}
    \item{Manure}{an ordered factor with levels}
    \item{Manure}{\code{0} < \code{1} < \code{2} < \code{3} < \code{4}}
  }
}
}
\source{
  Jongman, R.H.G, ter Braak, C.J.F & van Tongeren,
  O.F.R. (1987). \emph{Data Analysis in Community and Landscape
    Ecology}. Pudog, Wageningen.
}
}
\examples{
data(dune)
}
\keyword{datasets}
