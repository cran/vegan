\name{plot.cca}
\alias{plot.cca}
\alias{text.cca}
\alias{points.cca}
\alias{scores.cca}
\alias{scores.rda}
\alias{summary.cca}
\alias{print.summary.cca}
\alias{labels.cca}

\title{Plot or Extract Results of Constrained Correspondence Analysis
  or Redundancy Analysis}
\description{
  Functions to plot or extract results of constrained correspondence analysis
  (\code{\link{cca}}), redundancy analysis (\code{\link{rda}}), distance-based
  redundancy analysis (\code{\link{dbrda}}) or
  constrained analysis of principal coordinates (\code{\link{capscale}}).
}
\usage{
\method{plot}{cca}(x, choices = c(1, 2), display = c("sp", "wa", "cn"),
     scaling = "species", type, xlim, ylim, const,
     correlation = FALSE, hill = FALSE, cex = 0.7, ...)
\method{text}{cca}(x, display = "sites", labels, choices = c(1, 2),
     scaling = "species", arrow.mul, head.arrow = 0.05, select, const,
     axis.bp = FALSE, correlation = FALSE, hill = FALSE, ...)
\method{points}{cca}(x, display = "sites", choices = c(1, 2),
       scaling = "species", arrow.mul, head.arrow = 0.05, select, const,
       axis.bp = FALSE, correlation = FALSE, hill = FALSE, ...)
\method{scores}{cca}(x, choices = c(1,2), display = "all",
       scaling = "species", hill = FALSE, tidy = FALSE, droplist = TRUE,
       ...)
\method{scores}{rda}(x, choices = c(1,2), display = "all",
       scaling = "species", const, correlation = FALSE, tidy = FALSE,
       droplist = TRUE, ...)
\method{summary}{cca}(object, scaling = "species", axes = 6,
                      display=c("sp","wa","lc","bp","cn"),
                      digits = max(3, getOption("digits") - 3),
                      correlation = FALSE, hill = FALSE, ...)
\method{labels}{cca}(object, display, ...)
}

\arguments{
  \item{x, object}{A \code{cca} result object.}
    \item{choices}{Axes shown.}
  \item{display}{Scores shown.  These must include some of the
    alternatives \code{"species"} or \code{"sp"} for species scores,
    \code{sites} or \code{"wa"} for site scores, \code{"lc"} for linear
    constraints or LC scores, or \code{"bp"} for biplot arrows or
    \code{"cn"} for centroids of factor constraints instead of an arrow,
    and \code{"reg"} for regression coefficients (a.k.a. canonical
    coefficients). The alternative \code{"all"} selects all available
    scores.}
  \item{scaling}{Scaling for species and site scores. Either species
    (\code{2}) or site (\code{1}) scores are scaled by eigenvalues, and
    the other set of scores is left unscaled, or with \code{3} both are
    scaled symmetrically by square root of eigenvalues.  Corresponding
    negative values can be used in \code{cca} to additionally multiply
    results with \eqn{\sqrt(1/(1-\lambda))}.  This scaling is know as Hill
    scaling (although it has nothing to do with Hill's rescaling of
    \code{\link{decorana}}). With corresponding negative values
    in \code{rda}, species scores are divided by standard deviation of each
    species and multiplied with an equalizing constant. Unscaled raw
    scores stored in the result can be accessed with \code{scaling = 0}.

    The type of scores can also be specified as one of \code{"none"},
    \code{"sites"}, \code{"species"}, or \code{"symmetric"}, which
    correspond to the values \code{0}, \code{1}, \code{2}, and \code{3}
    respectively. Arguments \code{correlation} and \code{hill} in
    \code{scores.rda} and \code{scores.cca} respectively can be used in
    combination with these character descriptions to get the
    corresponding negative value.
  }
  \item{correlation, hill}{logical; if \code{scaling} is a character
    description of the scaling type, \code{correlation} or \code{hill}
    are used to select the corresponding negative scaling type; either
    correlation-like scores or Hill's scaling for PCA/RDA and CA/CCA
    respectively. See argument \code{scaling} for details.}
  
  \item{tidy}{Return scores that are compatible with
    \CRANpkg{ggplot2}: all scores are in a single \code{data.frame},
    score type is identified by factor variable \code{score}, the
    names by variable \code{label}, and weights (in CCA) are in
    variable \code{weight}. The possible values of \code{score} are
    \code{species}, \code{sites} (for WA scores), \code{constraints}
    (LC scores for sites calculated directly from the constraining
    variables), \code{biplot} (for biplot arrows), \code{centroids}
    (for levels of factor variables), \code{factorbiplot} (biplot
    arrows that model centroids), \code{regression} (for regression
    coefficients to find LC scores from constraints). These scores
    cannot be used with conventional \code{plot}, but they are
    directly suitable to be used with the \pkg{ggplot2} package.}

  \item{type}{Type of plot: partial match to \code{text}
    for text labels, \code{points} for points, and \code{none} for
    setting frames only.  If omitted, \code{text} is selected for
    smaller data sets, and \code{points} for larger.}
  \item{xlim, ylim}{the x and y limits (min,max) of the plot.}
  \item{labels}{Optional text to be used instead of row names. If you
    use this, it is good to check the default labels and their order
    using \code{labels} command.}
  \item{arrow.mul}{Factor to expand arrows in the graph.  Arrows will be
    scaled automatically to fit the graph if this is missing.}
  \item{head.arrow}{Default length of arrow heads.}
  \item{select}{Items to be displayed.  This can either be a logical
    vector which is \code{TRUE} for displayed items or a vector of indices
    of displayed items.}
  \item{const}{General scaling constant to \code{rda} scores. The
    default is to use a constant that gives biplot scores, that is,
    scores that approximate original data (see \code{\link{vignette}}
    on \sQuote{Design Decisions} with \code{browseVignettes("vegan")}
    for details and discussion). If \code{const} is a vector of two
    items, the first is used for species, and the second item for site
    scores.}
  \item{droplist}{Return a matrix instead of a named list when only one
    kind of scores were requested.}
  \item{axis.bp}{Draw \code{\link{axis}} for biplot arrows.}
  \item{axes}{Number of axes in summaries.}
  \item{digits}{Number of digits in output.}
  \item{cex}{Character expansion.}
  \item{...}{Parameters passed to other functions.}
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
  their parameters, allowing  a full user control of produced plots.

  Environmental variables receive a special treatment. With
  \code{display="bp"}, arrows will be drawn. These are labelled with
  \code{text} and unlabelled with \code{points}. The arrows have
  basically unit scaling, but if sites were scaled (\code{scaling}
  \code{"sites"} or \code{"symmetric"}), the scores of requested axes
  are adjusted relative to the axis with highest eigenvalue.  With
  \code{scaling = "species"} or \code{scaling = "none"}, the arrows will
  be consistent with vectors fitted to linear combination scores
  (\code{display = "lc"} in function \code{\link{envfit}}), but with
  other scaling alternatives they will differ. The basic \code{plot}
  function uses a simple heuristics for adjusting the unit-length arrows
  to the current plot area, but the user can give the expansion factor
  in \code{arrow.mul}.  With \code{display="cn"} the centroids of levels
  of \code{\link{factor}} variables are displayed. With this option continuous
  variables still are presented as arrows and ordered factors as arrows
  and centroids. With \code{display = "reg"} arrows will be drawn for
  regression coefficients (a.k.a. canonical coefficients) of constraints
  and conditions. Biplot arrows can be interpreted individually, but
  regression coefficients must be interpreted all together: the LC score
  for each site is the sum of regressions displayed by arrows. The
  partialled out conditions are zero and not shown in biplot arrows, but
  they are shown for regressions, and show the effect that must be
  partialled out to get the LC scores. The biplot arrows are more
  standard and more easily interpreted, and regression arrows should be
  used only if you know that you need them.

  If you want to have a better control of plots, it is best to
  construct the plot \code{text} and \code{points} commands which
  accept graphical parameters. It is important to remember to use the
  same \code{scaling}, \code{correlation} and \code{hill} arguments
  in all calls. The \code{plot.cca} command returns invisibly an
  \code{\link{ordiplot}} result object, and this will have consistent
  scaling for all its elements. The easiest way for full control of
  graphics is to first set up the plot frame using \code{plot} with
  \code{type = "n"} and all needed scores in \code{display} and save
  this result. The \code{points} and \code{text} commands for
  \code{\link{ordiplot}} will allow full graphical control (see
  section Examples). Utility function \code{labels} returns the default
  labels in the order they are applied in \code{text}.
  
  Palmer (1993) suggested using linear constraints (\dQuote{LC scores})
  in ordination diagrams, because these gave better results in
  simulations and site scores (\dQuote{WA scores}) are a step from
  constrained to unconstrained analysis.  However, McCune (1997) showed
  that noisy environmental variables (and all environmental measurements
  are noisy) destroy \dQuote{LC scores} whereas \dQuote{WA scores} were
  little affected.  Therefore the \code{plot} function uses site scores
  (\dQuote{WA scores}) as the default. This is consistent with the usage
  in statistics and other functions in \R (\code{\link[MASS]{lda}},
  \code{\link{cancor}}).  } \value{ The \code{plot} function returns
  invisibly a plotting structure which can be used by function
  \code{\link{identify.ordiplot}} to identify the points or other
  functions in the \code{\link{ordiplot}} family.  }

\author{Jari Oksanen }

\seealso{\code{\link{cca}}, \code{\link{rda}} and \code{\link{capscale}}
  for getting something
  to plot, \code{\link{ordiplot}} for an alternative plotting routine
  and more support functions, and \code{\link{text}},
  \code{\link{points}} and \code{\link{arrows}} for the basic routines.  }

\note{Up to \pkg{vegan} release 2.6-4 \code{summary} printed
  ordination scores, and function was occasionally used to extract
  scores. This is now deprecated and you should use
  \code{scores}. This version of \pkg{vegan} still enables extracting
  scores with \code{summary}, but you should change your code to use
  \code{scores} before next \pkg{vegan} release. For instructions,
  see \url{https://github.com/vegandevs/vegan/discussions/644}.  }

\examples{
data(dune)
data(dune.env)
mod <- cca(dune ~ A1 + Moisture + Management, dune.env)
## better control -- remember to set scaling etc identically
plot(mod, type="n", scaling="sites")
text(mod, dis="cn", scaling="sites")
points(mod, pch=21, col="red", bg="yellow", cex=1.2, scaling="sites")
text(mod, "species", col="blue", cex=0.8, scaling="sites")
## catch the invisible result and use ordiplot support - the example
## will make a biplot with arrows for species and correlation scaling
pca <- rda(dune)
pl <- plot(pca, type="n", scaling="sites", correlation=TRUE)
with(dune.env, points(pl, "site", pch=21, col=1, bg=Management))
text(pl, "sp", arrow=TRUE, length=0.05, col=4, cex=0.6, xpd=TRUE)
with(dune.env, legend("bottomleft", levels(Management), pch=21, pt.bg=1:4, bty="n"))
## Scaling can be numeric or more user-friendly names
## e.g. Hill's scaling for (C)CA
scrs <- scores(mod, scaling = "sites", hill = TRUE)
## or correlation-based scores in PCA/RDA
scrs <- scores(rda(dune ~ A1 + Moisture + Management, dune.env),
               scaling = "sites", correlation = TRUE)
}
\keyword{hplot}
\keyword{aplot}
