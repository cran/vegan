\name{ordixyplot}
\alias{ordixyplot}
\alias{panel.ordi}
\alias{panel.ordiarrows}
\alias{ordilattice.getEnvfit}

\title{ Trellis (Lattice) Plots for Ordination }

\description{
  Function \code{ordixyplot} provides an interface to plot ordination
  results using Trellis function \code{\link[lattice]{xyplot}} in
  package \pkg{lattice}.
}

\usage{
ordixyplot(x, data = NULL, formula, display = "sites", choices = 1:3,
    panel = "panel.ordi", aspect = "iso", envfit,
    type = c("p", "biplot"), ...)
}

\arguments{
  \item{x}{ An ordination result that \code{\link{scores}} knows: any
    ordination result in \pkg{vegan} and many others. }
  \item{data}{ Optional data to amend ordination results. The ordination
    results are found from \code{x}, but you may give here data for other
    variables needed in plots. Typically these are environmental data.}
  \item{formula}{ Formula to define the plots. A default formula will be
    used if this is omitted. The
    ordination axes must be called by the same names as in the
    ordination results (and these names vary among methods). }
  \item{display}{ The kind of scores: an argument passed to
    \code{\link{scores}}. }
  \item{choices}{ The axes selected: an argument passed to
    \code{\link{scores}}. } 
  \item{panel}{ The name of the panel function. } 
  \item{aspect}{The aspect of the plot (passed to the \pkg{lattice}
    function).}
  \item{envfit}{Result of \code{\link{envfit}} function displayed in
    \code{ordixyplot}. Please note that this needs same \code{choices}
    as \code{ordixyplot}.}
  \item{type}{The type of plot. This knows the same alternatives as
    \code{\link[lattice]{panel.xyplot}}. In addition \code{ordixyplot}
    has alternatives \code{"biplot"}, \code{"arrows"} and
    \code{"polygon"}. The first displays fitted vectors and factor
    centroids of \code{envfit}, or in constrained ordination, the biplot
    arrows and factor centroids if \code{envfit} is not given. The
    second (\code{type = "arrows"}) is a trellis variant of
    \code{\link{ordiarrows}} and draws arrows by \code{groups}. The line
    parameters are controlled by \code{\link[lattice]{trellis.par.set}}
    for \code{superpose.line}, and the user can set \code{length},
    \code{angle} and \code{ends} parameters of
    \code{\link[lattice]{panel.arrows}}.  The last one (\code{type =
    "polygon"}) draws a polygon enclosing all points in a panel over a
    polygon enclosing all points in the data. The overall polygon is
    controlled by Trellis parameters \code{\link[lattice]{trellis.par.set}}
    \code{plot.polygon} and \code{superpose.polygon}.}
  \item{\dots}{ Arguments passed to \code{\link{scores}} methods or
    \pkg{lattice} functions.}
}
\details{
  
  The function provides an interface to the corresponding \pkg{lattice}
  function.  All graphical parameters are passed to the \pkg{lattice}
  function so that these graphs are configurable. See
  \code{\link[lattice]{Lattice}} and \code{\link[lattice]{xyplot}}
  for details, usage and possibilities.

  The argument \code{x} must always be an ordination result. The scores
  are extracted with \pkg{vegan} function \code{\link{scores}} so that
  these functions work with all \pkg{vegan} ordinations and many others.

  The \code{formula} is used to define the models. Function has a
  simple default formula which is used if \code{formula} is missing. 
  The formula must use the names of ordination scores and names
  of \code{data}.

  The ordination scores are found from \code{x}, and \code{data} is
  optional. The \code{data} should contain other variables than
  ordination scores to be used in plots. Typically, they are
  environmental variables (typically factors) to define panels or plot
  symbols.

  The proper work is done by the panel function. The layout can be
  changed by defining own panel functions. See
  \code{\link[lattice]{panel.xyplot}} for details and survey of
  possibilities.

  Ordination graphics should always be isometric: same scale should be
  used in all axes. This is controlled (and can be changed) with
  argument \code{aspect} in \code{ordixyplot}.
}

\value{
  The function return \code{\link[lattice]{Lattice}} objects of class
  \code{"trellis"}.   
}
\author{Jari Oksanen }
 
\seealso{
  \code{\link[lattice]{Lattice}}, \code{\link[lattice]{xyplot}}.
}

\note{
  \pkg{vegan} releases 2.6-10 and earlier had lattice functions
  \code{\link{ordicloud}} and \code{\link{ordisplom}} which are now
  deprecated. However, \CRANpkg{vegan3d} (version 1.4-0 and later) has
  function \code{ordilattice3d} which is equal to
  \code{ordicloud}.
}

\examples{
data(dune, dune.env)
ord <- cca(dune)
## Scatter plot with polygons
ordixyplot(ord, data=dune.env, form = CA1 ~ CA2 | Management,
  groups=Manure, type = c("p","polygon"))
## Choose a different scaling
ordixyplot(ord, scaling = "sites")
## ... Slices of third axis
ordixyplot(ord, form = CA1 ~ CA2 | lattice::equal.count(CA3, 4),
   type = c("g","p", "polygon"))
## Display environmental variables
ordixyplot(ord, envfit = envfit(ord ~ Management + A1, dune.env, choices=1:3))
}

\keyword{ hplot }

