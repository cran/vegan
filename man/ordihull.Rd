\name{ordihull}
\alias{ordihull}
\alias{ordiarrows}
\alias{ordisegments}
\alias{ordigrid}
\alias{ordispider}
\alias{ordiellipse}
\alias{weights.cca}
\alias{weights.rda}
\alias{weights.decorana}

\title{Add Graphical Items to Ordination Diagrams}
\description{
  Functions to add convex hulls, arrows, line segments, regular grids of
  points, `spider' graphs or ellipses to ordination diagrams. The
  ordination diagrams can be produced by \code{vegan}
  \code{\link{plot.cca}}, \code{\link{plot.decorana}} or
  \code{\link{ordiplot}}.
}
\usage{
ordihull(ord, groups, display = "sites", draw = c("lines","polygon"), ...)
ordiarrows(ord, groups, levels, replicates, display = "sites", ...)
ordisegments(ord, groups, levels, replicates, display = "sites", ...)
ordigrid(ord, levels, replicates, display = "sites",  ...)
ordispider(ord, groups, display="sites", w = weights(ord, display), ...)
ordiellipse(ord, groups, display="sites", kind = c("sd","se"), conf,
            draw = c("lines","polygon"), w = weights(ord, display), ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{ord}{An ordination object or an \code{\link{ordiplot}} object. }
  \item{groups}{Factor giving the groups for which the graphical item is
    drawn. }
  \item{levels, replicates}{Alternatively, regular
    groups can be defined with arguments \code{levels} and
    \code{replicates}, where \code{levels} gives the number of groups,
    and \code{replicates} the number of successive items at the same
    group.}
  \item{display}{Item to displayed. }
  \item{draw}{Use either \code{\link{lines}} or \code{\link{polygon}} to
    draw the
    line. Graphical parameters are passed to both. The main difference
    is that \code{polygon}s may be filled and non-transparent.}
  \item{w}{Weights used to find the average within group. Weights are
    used automatically for \code{\link{cca}}
    and \code{\link{decorana}} results, unless undone by the
    user. \code{w=NULL} sets equal weights to all points. }
  \item{kind}{Wheter standard deviations of points (\code{sd}) or
    standard deviations of their (weighted) averages (\code{se}) are
    used.}
  \item{conf}{Confidence limit for ellipses, e.g. 0.95. If given, the
    corresponding \code{sd} or \code{se} is multiplied with the
    corresponding value found from the Chi-squared distribution with 2df. }
  \item{\dots}{Parameters passed to graphical functions such
    as \code{\link{lines}}, \code{\link{segments}}
    \code{\link{arrows}}, \code{\link{polygon}} or to
    \code{\link{scores}} to select axes and
    scaling etc. }
  }
\details{
  Function \code{ordihull} draws \code{\link{lines}} or
  \code{\link{polygon}}s for the convex
  hulls found by function \code{\link{chull}} encircling
  the items in the groups.

  Function \code{ordiarrows} draws
  \code{\link{arrows}} and \code{ordisegments} draws line
  \code{\link{segments}} between successive items in the
  groups. Function \code{ordigrid} draws line
  \code{\link{segments}} both within the groups and for the
  corresponding items among the groups.

  Function \code{ordispider} draws a `spider' diagram where each point
  is connected to the group centroid with
  \code{\link{segments}}. Weighted centroids are used in the
  correspondence analysis  methods \code{\link{cca}} and
  \code{\link{decorana}} or if the user gives the weights in the
  call. If \code{ordispider} is called with \code{\link{cca}} or
  \code{\link{rda}} result without \code{groups} argument, the function
  connects each `WA' scores to the correspoding `LC' score.

  Function \code{ordiellipse} draws \code{\link{lines}} or
  \code{\link{polygon}}s for dispersion
  \code{\link[ellipse]{ellipse}} using either standard deviation of
  point scores or standard error of the (weighted) average of
  scores, and the (weighted) correlation defines the direction of the
  principal axis of the ellipse. The function requires library
  \code{ellipse}.   
}

\note{These functions add graphical items to ordination graph: You must
  draw a graph first.
  }
\author{ Jari Oksanen }

\seealso{The function pass parameters to basic graphical functions, and
  you may wish to change the default values in \code{\link{arrows}},
  \code{\link{lines}}, \code{\link{segments}} and
  \code{\link{polygon}}. You can pass
  parameters to \code{\link{scores}} as well. Other underlying functions
  are \code{\link{chull}} and \code{\link[ellipse]{ellipse}}.
}


\examples{
data(dune)
data(dune.env)
mod <- cca(dune ~ Moisture, dune.env)
attach(dune.env)
plot(mod, type="n")
ordihull(mod, Moisture)
ordispider(mod, col="red")
# The following is not executed automatically because it needs
# a non-standard library `ellipse'. 
\dontrun{ordiellipse(mod, Moisture, kind="se", level=0.95, lwd=2, col="blue")}
}
\keyword{aplot }

