\name{ordiplot}
\alias{ordiplot}
\alias{identify.ordiplot}

\title{ Alternative Point plot and identify Functions for Ordination }
\description{
  Ordination plot function especially for congested plots. Function
  \code{ordiplot} always plots only unlabelled points, but
  \code{identify.ordiplot} can be used to add labels to selected site,
  species or constraint points.
}
\usage{
ordiplot(ord, choices = c(1, 2), ...)
\method{identify}{ordiplot}(x, what = c("sites", "species", "constraints"), ...) 
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{ord}{A result from an ordination.}
  \item{choices}{Axes shown. }
  \item{\dots}{Other graphcial parameters. }
  \item{x}{A result object from \code{ordiplot}.}
  \item{what}{Items identified in the ordination plot.}
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
  but you can call directly the \code{\link{identify}} and
  \code{\link{plot}} functions for stronger user control.
}

\seealso{ \code{\link{identify}} for basic operations, \code{\link{plot.cca}},
  \code{\link{plot.decorana}} which also produce objects for
  \code{identify.ordiplot} and \code{\link{scores}} for extracting
  scores from non-\code{vegan} ordinations.
}

\examples{
# The example needs user interaction and is not run automatically
\dontrun{data(varespec)}
\dontrun{ordina <- cca(varespec)}
\dontrun{tmp <- ordiplot(ordina)}
\dontrun{identify(tmp, "spec")}
}q
\keyword{ hplot }
\keyword{ iplot }
