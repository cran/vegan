\name{plot.cca}
\alias{plot.cca}
\alias{text.cca}
\alias{points.cca}
\alias{scores.cca}

\title{Plot or Extract Results of Constrained Correspondence Analysis
  or Redundancy Analysis}
\description{
  Functions to plot or extract results of constrained correspondence analysis
  (\code{\link{cca}}) or redundancy analysis (\code{\link{rda}}).
}
\usage{
\method{plot}{cca}(x, choices = c(1, 2), display = c("sp", "wa", "bp"),
         scaling = 2, type, ...)
\method{text}{cca}(x, display = "sites", choices = c(1, 2), scaling = 2,
    mul.arrow = 1, head.arrow = 0.05, ...)
\method{points}{cca}(x, display = "sites", choices = c(1, 2), scaling = 2,
    mul.arrow = 1, head.arrow = 0.05, ...)
\method{scores}{cca}(x, choices=c(1,2), display=c("sp","wa","bp"),scaling=2, ...)
}

\arguments{
  \item{x}{A \code{cca} result object.}
    \item{choices}{Axes shown.}
  \item{display}{Scores shown.  These must some of the alternatives
    \code{sp} for species scores, \code{wa} for site scores, \code{lc}
    for linear constraints or ``LC scores'', or \code{bp} for biplot
    arrows or \code{cn} for centroids of factor constraints instead of
    an arrow.}
  \item{type}{Type of plot: partial match to \code{text}
    for text labels, \code{points} for points, and \code{none} for
    setting frames only.  If omitted, \code{text} is selected for
    smaller data sets, and \code{points} for larger.}
  \item{scaling}{Scaling for species and site scores. Either species
    (\code{2}) or site (\code{1}) scores are scaled by eigenvalues, and
    the other set of scores is left unscaled, or with \code{3} both are
    scaled symmetrically by square root of eigenvalues. }
  \item{mul.arrow}{Factor to expand arrows to fit the graph.}
  \item{head.arrow}{Default length of arrow heads.}
  \item{...}{Other parameters for plotting functions.}
}

\details{
  Same \code{plot} function will be used for \code{\link{cca}} and
  \code{\link{rda}}. This produces a quick, standard plot with current
  \code{scaling}.

  The \code{plot} function sets colours (\code{col}), plotting
  characters (\code{pch}) and character sizes (\code{cex}) to
  certain standard values. For a fuller control of produced plot, it is
  best to call \code{plot} with \code{type="none"} first, and then add
  each plotting item separately using \code{text.cca} or
  \code{points.cca} functions. These use the default settings of standard
  \code{\link{text}} and \code{\link{points}} functions and accept all
  their parameters, allowing thus a full user control of produced plots.

  Environmental variables receive a special treatment. With
  \code{display="bp"}, arrows will be drawn. These are labeled with
  \code{text} and unlabeled with \code{points}. The basic \code{plot}
  function uses a simple (but not very clever) heuristics for adjusting
  arrow lengths to plots, but with \code{points.cca} and \code{text.cca}
  the user must give the expansion factor in
  \code{mul.arrow}. The behaviour is still more peculiar with
  \code{display="cn"} which requests centroids of levels of
  \code{\link{factor}} variables (these are available only if there were
  factors and a formula interface was used in \code{\link{cca}} or
  \code{\link{rda}}). With this option, biplot arrows are plotted as
  well for cases which do not have a centroid: Continuous variables are
  presented with arrows and ordered factors with arrows and centroids.

  If you want to have still a better control of plots, it is better to
  produce them by hand. Function \code{scores} helps in extracting the
  needed components with the selected \code{scaling}.
}
\value{
  The \code{plot} function returns invisibly a plotting structure which
  can be used by function \code{\link{identify.ordiplot}} to identify
  the points or other functions in the \code{\link{ordiplot}} family. 
}

\author{Jari Oksanen }
\note{ Option \code{display="cn"} (centroids and biplot arrows) may
  become the default instead of the current \code{display="bp"} in the
  future version.
  }

\seealso{\code{\link{cca}} and \code{\link{rda}} for getting something
  to plot, \code{\link{ordiplot}} for an alternative plotting routine
  and more support functions, and \code{\link{text}},
  \code{\link{points}} and \code{\link{arrows}} for the basic routines.  }

\examples{
data(dune)
data(dune.env)
mod <- cca(dune ~ A1 + Moisture + Management, dune.env)
plot(mod, type="n")
text(mod, dis="cn", mul=2)
points(mod, pch=21, col="red", bg="yellow", cex=1.2)
text(mod, "species", col="blue", cex=0.8)
}
\keyword{hplot}
\keyword{aplot}
