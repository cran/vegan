\name{vegemite}
\alias{vegemite}
\alias{coverscale}

\title{Prints a Compact, Ordered Vegetation Table }
\description{
  The function prints a compact vegetation table, where species are
  rows, and each site takes only one column without spaces.  The
  vegetation table can be ordered by explicit indexing, by environmental
  variables or results from an ordination or cluster analysis.
}
\usage{
vegemite(x, use, scale, sp.ind, site.ind, zero=".")
coverscale(x, scale=c("Braun.Blanquet", "Domin", "Hult", "Hill", "fix", "log"))
}

\arguments{
  \item{x}{Vegetation data. }
  \item{use}{Either a vector or an object from \code{cca},
    \code{decorana} \emph{etc.} or \code{hclust} for ordering sites and species.}
  \item{sp.ind}{Species indices. }
  \item{site.ind}{Site indices. }
  \item{zero}{Character used for zeros. }
  \item{scale}{Cover scale used (can be abbreviated).}
}
\details{
  The function prints a traditional vegetation table.
  Unlike in ordinary data matrices, species are used as rows and sites
  as columns.  The table is printed in compact form:  only one character
  can be used for abundance, and there are no spaces between columns.

  The parameter \code{use} can be a vector or an object from
  \code{\link[mva]{hclust}} or any ordination result recognized by
  \code{\link{scores}}. 
  If \code{use} is a vector, it is used
  for ordering sites.  If \code{use} is an object from ordination, both
  sites and species
  are arranged by the first axis.  
  When \code{use} is an
  object from \code{\link[mva]{hclust}}, the sites are ordered similarly
  as in the cluster dendrogram.
  If ordination methods provide species scores, these are used for
  ordering species.  In all cases where species scores are missing,
  species are ordered by their weighted averages (\code{\link{wascores}})
  on site scores. There is no natural, unique ordering in hierarchic
  clustering, but in some cases species are still nicely ordered.
  Alternatively, species and sites can be ordered explicitly giving
  their indices or names in parameters \code{sp.ind} and
  \code{site.ind}.  If these are given, they take precedence over
  \code{use}. 

  If \code{scale} is given, \code{vegemite} calls
  \code{coverscale} to transform percent cover
  scale or some other scales into traditional class scales used in
  vegetation science (\code{coverscale} can be called directly, too).
  Braun-Blanquet and Domin scales are actually not
  strict cover scales, and the limits used for codes \code{r} and
  \code{+} are arbitrary.  Scale \code{Hill} may be
  inappropriately named, since Mark O. Hill probably never intended this
  as a cover scale.  However, it is used as default `cut levels' in his
  \code{TWINSPAN}, and surprisingly many users stick to this default,
  and so this is a \emph{de facto} standard in publications.  All
  traditional
  scales assume that values are cover percentages with maximum 100.
  However, non-traditional alternative \code{log} can be used with any
  scale range.  Its class limits are integer powers of 1/2 of the
  observed maximum in the data, with \code{+} used for non-zero entries
  less than 1/512 of data maximum (\code{log} stands alternatively for
  logarithmic or logical).  Scale \code{fix} is intended for `fixing'
  10-point scales: it truncates scale values to integers, and replaces
  10 with \code{X} and positive values below 1 with \code{+}. 
}
\value{
  The function is used mainly to print a table, but it returns
  (invisibly) a list
  with items:
  \item{spec}{Ordered species indices.}
  \item{sites}{Ordered site indices.}
}
\references{ The cover scales are presented in many textbooks of vegetation
  science; I used:

  Shimwell, D.W. (1971) \emph{The Description and Classification of
  Vegetation}. Sidgwick & Jackson.
}
\author{Jari Oksanen}

\seealso{\code{\link{cut}} and \code{\link{approx}} for making your own
  `cover scales', \code{\link{wascores}} for weighted averages.
  }

\note{ This function was called \code{vegetab} in older versions of
  \code{vegan}.  The new name was chosen  because the output is so
  compact (and to avoid confusion with the \code{vegtab} function in the
  \code{labdsv} package).
    }
\examples{
data(varespec)
## Print only more common species 
freq <- apply(varespec > 0, 2, sum)
vegemite(varespec, scale="Hult", sp.ind = freq > 10)
## Order by correspondence analysis, use Hill scaling and layout:
dca <- decorana(varespec)
vegemite(varespec, dca, "Hill", zero="-")
}
\keyword{ print }
\keyword{ manip }

