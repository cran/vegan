\name{surf}
\alias{surf}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Smooths Variables and Plots Contours on Ordination. }
\description{
  Function \code{surf} fits a smooth surface for given variable and
  plots the result on ordination diagram.
}
\usage{
surf(x, y, choices=c(1, 2), knots=10, family="gaussian", col="red",
     thinplate = TRUE, add = FALSE, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{Ordination configuration, either a matrix or a result known
    by \code{\link{scores}}. }
  \item{y}{ Variable to be plotted. }
  \item{choices}{Ordination axes. }
  \item{knots}{Number of initial knots in \code{\link[mgcv]{gam}} (one
    more than degrees of freedom). }
  \item{family}{ Error distribution in  \code{\link[mgcv]{gam}}. }
  \item{col}{ Colour of contours. }
  \item{thinplate}{Use thinplate splines in \code{\link[mgcv]{gam}}.}
  \item{add}{Add contours on an existing diagram or draw a new plot. }
  \item{\dots}{ Other graphical parameters. }
}
\details{
  Function \code{surf} fits a smooth surface using thinplate spline
  fitting in \code{\link[mgcv]{gam}}, and  interpolates the fitted
  values  into a
  regular grid using \code{\link[akima]{interp}}.  Finally, it plots the
  results either over an existing ordination diagram or draws a new plot
  with sample plots and fitted contours.  The function uses
  \code{\link{scores}} to extract ordination scores, and \code{x} can be
  any result object known by that function.
}
\value{
  Function is usually called for its side effect of drawing the contour
  plot, but it returns the result object of \code{\link[mgcv]{gam}}.
}
\author{ Dave Roberts and Jari Oksanen }
\note{ The funtion requires libraries \code{mgcv}
  (\code{\link[mgcv]{gam}}) and \code{akima}
  (\code{\link[akima]{interp}}).  In fact, it is a very primitive wrapper
  for these.

  The default is to use thinplate splines.  These make sense in
  ordination as they have equal smoothing in all directions and are
  rotation invariant.  However, they seem to fail badly in some case,
  and then separate spline smoothing may be used.
}


\seealso{ For basic routines \code{\link[mgcv]{gam}},
  \code{\link[akima]{interp}} \code{\link{scores}}, and
  \code{\link{envfit}} for a poorer but more traditional and compact
  alternative. } 

\examples{
## The examples are not run by `example(surf)' because they need
## libraries `mgcv' and `akima' which may not exist in every system.
\dontrun{data(varespec)}
\dontrun{data(varechem)}
\dontrun{library(MASS)}
\dontrun{library(mva)}
\dontrun{vare.dist <- vegdist(varespec)}
\dontrun{vare.mds <- isoMDS(vare.dist)}
\dontrun{attach(varespec)}
\dontrun{attach(varechem)}
\dontrun{surf(vare.mds, Baresoil, xlab="Dim1", ylab="Dim2")}
## Total cover of reindeer lichens
\dontrun{surf(vare.mds, Cla.ste+Cla.arb+Cla.ran, xlab="Dim1", ylab="Dim2")} 
}
\keyword{ multivariate }% at least one, from doc/KEYWORDS
\keyword{ aplot }
