\name{ordiplot}
\alias{ordiplot}
\alias{identify.ordiplot}
\alias{scores.ordiplot}

\title{ Alternative plot and identify Functions for Ordination }
\description{
  Ordination plot function especially for congested plots. Function
  \code{ordiplot} always plots only unlabelled points, but
  \code{identify.ordiplot} can be used to add labels to selected site,
  species or constraint points.  Function \code{identify.ordiplot} can
  be used to identify points from \code{\link{plot.cca}},
  \code{\link{plot.decorana}} or \code{\link{plot.procrustes}} as well.
}
\usage{
ordiplot(ord, choices = c(1, 2), type="points", ...)
\method{identify}{ordiplot}(x, what, ...)
}

\arguments{
  \item{ord}{A result from an ordination.}
  \item{choices}{Axes shown. }
  \item{type}{The type of graph which may be \code{"points"} or
    \code{"none"} for any ordination method, or any of the alternatives
    in \code{\link{plot.cca}} or \code{\link{plot.decorana}} in
    \code{\link{cca}}, \code{\link{rda}} or \code{\link{decorana}}
    graphs.}
  \item{\dots}{Other graphcial parameters. }
  \item{x}{A result object from \code{ordiplot}.}
  \item{what}{Items identified in the ordination plot. The types depend
    on the kind of plot used. Most methods know \code{sites} and
    \code{species}, functions \code{\link{cca}} and \code{\link{rda}}
    know in addition 
    \code{constraints} (for `LC' scores), \code{centroids} and
    \code{biplot}, and \code{\link{plot.procrustes}} ordination plot has
    \code{heads} and \code{points}.}
}
\details{
  Function \code{ordiplot} draws an ordination diagram using black circles for
  sites and red crosses for species.  It returns invisibly an object of
  class \code{"ordiplot"} which can be used by \code{identify.ordiplot}
  to label selected sites or species, or constraints in
  \code{\link{cca}} and \code{\link{rda}}.

  The function can handle output from several alternative ordination
  methods. For \code{\link{cca}}, \code{\link{rda}} and
  \code{\link{decorana}} it uses their \code{plot} method with option
  \code{type = "points"}. In addition, the \code{plot} functions of
  these methods return invisibly an \code{"ordiplot"} object which can
  be used by \code{identify.ordiplot} to label points. For other
  ordinations it relies on \code{\link{scores}} to extract the scores.
}
\value{
  Function \code{ordiplot} returns invisibly an object of class
  \code{"ordiplot"} with items \code{sites}, \code{species} and
  \code{constraints} (if these are available in the ordination
  object). Function \code{identify.ordiplot} uses this object to label
  the point.
}

\author{
  Jari Oksanen
}
\note{
  The purpose of these functions is to provide similar functionality as
  the \code{plot}, \code{plotid} and \code{specid} methods in library
  \code{labdsv}. The functions are somewhat limited in parametrization,
  but you can call directly the standard \code{\link{identify}} and
  \code{\link{plot}} functions for a better user control.
}

\seealso{ \code{\link{identify}} for basic operations, \code{\link{plot.cca}},
  \code{\link{plot.decorana}}, \code{\link{plot.procrustes}} which also
  produce objects for
  \code{identify.ordiplot} and \code{\link{scores}} for extracting
  scores from non-\code{vegan} ordinations.   
}

\examples{
data(dune)
ordina <- cca(dune)
tmp <- ordiplot(ordina)
# identify needs user interaction
\dontrun{identify(tmp, "spec")}
}
\keyword{ hplot }
\keyword{ iplot }
