\name{linestack}
\alias{linestack}

\title{Plots One-dimensional Labelled Diagrams without Overwriting Labels }
\description{
  Function \code{linestack} plots vertical one-dimensional plots for
  numeric vectors.  The plots are always labelled, but the the labels
  are moved vertically to avoid overwriting.
}
\usage{
linestack(x, cex = 0.8, label = "right", hoff = 2, air = 1.1, at = 0,
    add = FALSE, axis = FALSE, ...)
}

\arguments{
  \item{x}{Numeric vector to be plotted. }
  \item{cex}{Size of the labels. }
  \item{label}{Put labels to the \code{"right"} or
    \code{"left"} of the axis. }
  \item{hoff}{Distance from the vertical axis to the label in units of
    the width of letter \dQuote{m}. }
  \item{air}{Multiplier to string height to leave empty space between labels. }
  \item{at}{Position of plot in horizontal axis. }
  \item{add}{Add to an existing plot. }
  \item{axis}{Add axis to the plot. }
  \item{\dots}{Other graphical parameters to labels.}
}
}
\value{
  The function draws a plot and returns nothing useful.
}
\author{Jari Oksanen }
\note{ The function always draws labelled diagrams.  If you want to have
  unlabelled diagrams, you can use, e.g., \code{\link{plot}},
  \code{\link{stripchart}} or \code{\link{rug}}.
}

\examples{
## First DCA axis
data(dune)
ord <- decorana(dune)
linestack(scores(ord, choices=1, display="sp"))
linestack(scores(ord, choices=1, display="si"), label="left", add=TRUE)
title(main="DCA axis 1")
}
\keyword{ hplot }
\keyword{ aplot }
