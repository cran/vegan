\name{distconnected}
\alias{distconnected}
\alias{no.shared}
\alias{spantree}

\title{Connectedness and Minimum Spanning Tree for Dissimilarities }
\description{
  Function \code{distconnected} finds groups that are connected
  disregarding dissimilarities that are at or above a threshold or
  \code{NA}. The function can be used to find groups that can be
  ordinated together or transformed by
  \code{\link{stepacross}}. Function \code{no.shared} returns a logical
  dissimilarity object, where \code{TRUE} means that sites have no
  species in common. This is a minimal structure for
  \code{distconnected} or can be used to set missing values to
  dissimilarities.
  Function \code{spantree} finds a minimum spanning tree
  connecting all points, but disregarding dissimilarities that are at or
  above the threshold or \code{NA}.  
}
\usage{
distconnected(dis, toolong = 1, trace = TRUE)
no.shared(x)
spantree(dis, toolong = 1)
}

\arguments{
  \item{dis}{Dissimilarity data inheriting from class \code{dist} or
    a an object, such as a matrix, that can be converted to a
    dissimilarity matrix. Functions \code{\link{vegdist}} and
    \code{\link[mva]{dist}} are some functions producing suitable
    dissimilarity data.}
  \item{toolong}{ Shortest dissimilarity regarded as \code{NA}.
    The function uses a fuzz factor, so
    that dissimilarities close to the limit will be made \code{NA}, too. }
  \item{trace}{Summarize results of \code{distconnected}}
  \item{x}{Community data.}
  
}
\details{
  Data sets are disconnected if they have sample plots or groups of
  sample plots which share no species with other sites or groups of
  sites. Such data sets
  cannot be sensibly ordinated by any unconstrained method, because
  these subsets cannot be related to each other. For instance,
  correspondence analysis will polarize these subsets with eigenvalue
  1. Neither can such dissimilarities be transformed with
  \code{\link{stepacross}}, because there is no path between all points,
  and result will contain \code{NA}s. Function \code{distconnected} will
  find such subsets in dissimilarity matrices. The function will return
  a grouping vector that can be used for subsetting the
  data. If data are connected, the result vector will be all
  \eqn{1}s. The connectedness between two points can be defined either
  by a threshold \code{toolong} or using input dissimilarities
  with \code{NA}s. If \code{toolong} is zero or negative, no
  threshold will be used.

  Function \code{no.shared} returns a \code{dist} structure having value
  \code{TRUE} when two sites have nothing in common, and value
  \code{FALSE} when they have at least one shared species. This is a
  minimal structure that can be analysed with \code{distconnected}. The
  function can be used to select dissimilarities with no shared species
  in indices which do not have a fixed upper limit.
  
  Function \code{spantree} finds a minimum spanning tree for
  dissimilarities (there may be several minimum spanning trees, but the
  function finds only one). Dissimilarities at or above the threshold
  \code{toolong} and \code{NA}s are disregarded, and the spanning tree
  is found through other dissimilarities. If the data are disconnected,
  the function will return a disconnected tree (or a forest), and the
  corresponding link is \code{NA}. The results of \code{spantree} can be
  overlaid onto an ordination diagram using function
  \code{\link{ordispantree}}. 

  Function \code{distconnected} uses depth-first search
  (Sedgewick 1990). Function \code{spantree} uses Prim's method
  implemented as priority-first search for dense graphs (Sedgewick
  1990). 
}
\value{
  Function \code{distconnected} returns a vector for
  observations using integers to identify connected groups. If the data
  are connected, values will be all \code{1}. Function \code{no.shared}
  returns an object of class \code{\link[mva]{dist}}. Function \code{spantree}
  returns a list with two vectors, each of length \eqn{n-1}. The
  number of links in a tree is one less the number of observations, and
  the first item is omitted. The items are  
  \item{kid }{The child node of the parent, starting from parent number
    two. If there is no link from the parent, value will be \code{NA}
    and tree is disconnected at the node.}
  \item{dist }{Corresponding distance. If \code{kid = NA}, then
    \code{dist = 0}.}
}
\references{
 Sedgewick, R. (1990). \emph{Algorithms in C}. Addison Wesley. 
}
\author{ Jari Oksanen }
\note{
  In principle, minimum spanning tree is equivalent to single linkage
  clustering that can be performed using \code{\link[mva]{hclust}} or
  \code{\link[cluster]{agnes}}. However, these functions combine
  clusters to each other and the information of the actually connected points
  (the ``single link'') cannot be recovered from the result. The
  graphical output of a single linkage clustering plotted with
  \code{\link{ordicluster}} will look very different from an equivalent
  spanning tree plotted with \code{\link{ordispantree}}.
}


\seealso{\code{\link{vegdist}} or \code{\link[mva]{dist}} for getting
  dissimilarities, \code{\link{stepacross}} for a case where you may need
    \code{distconnected}, \code{\link{ordispantree}} for displaying
    results of \code{spantree}, and \code{\link[mva]{hclust}} or
    \code{\link[cluster]{agnes}} for single linkage clustering. 
}
\examples{
## There are no disconnected data in vegan, and the following uses an
## extremely low threshold limit for connectedness. This is for
## illustration only, and not a recommended practice.
data(dune)
dis <- vegdist(dune)
ord <- cmdscale(dis) ## metric MDS
gr <- distconnected(dis, toolong=0.4)
tr <- spantree(dis, toolong=0.4)
ordiplot(ord, type="n")
ordispantree(ord, tr, col="red", lwd=2)
points(ord, cex=1.3, pch=21, col=1, bg = gr)
# Make sites with no shared species as NA in Manhattan dissimilarities
dis <- vegdist(dune, "manhattan")
is.na(dis) <- no.shared(dune)
}
\keyword{ multivariate}

