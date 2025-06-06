\encoding{UTF-8}
\name{designdist}
\alias{designdist}
\alias{designdist2}
\alias{chaodist}

\title{Design your own Dissimilarities }
\description{

  Function \code{designdist} lets you define your own dissimilarities
  using terms for shared and total quantities, number of rows and number
  of columns. The shared and total quantities can be binary, quadratic
  or minimum terms. In binary terms, the shared component is number of
  shared species, and totals are numbers of species on sites. The
  quadratic terms are cross-products and sums of squares, and minimum
  terms are sums of parallel minima and row totals. Function
  \code{designdist2} is similar, but finds dissimilarities among two
  data sets. Function \code{chaodist} lets you define your own
  dissimilarities using terms that are supposed to take into account the
  \dQuote{unseen species} (see Chao et al., 2005 and Details in
  \code{\link{vegdist}}).

}
\usage{
designdist(x, method = "(A+B-2*J)/(A+B)",
           terms = c("binary", "quadratic", "minimum"), 
           abcd = FALSE, alphagamma = FALSE, name, maxdist)
designdist2(x, y, method = "(A+B-2*J)/(A+B)",
           terms = c("binary", "quadratic", "minimum"),
           abcd = FALSE, alphagamma = FALSE, name, maxdist)
chaodist(x, method = "1 - 2*U*V/(U+V)", name)
}

\arguments{
  \item{x}{Input data.}
  \item{y}{Another input data set: dissimilarities will be calculated
    among rows of \code{x} and rows of \code{y}.}
  \item{method}{Equation for your dissimilarities. This can use terms
    \code{J} for shared quantity, \code{A} and \code{B} for totals,
    \code{N} for the number of rows (sites) and \code{P} for the
    number of columns (species) or in \code{chaodist} it can use terms
    \code{U} and \code{V}. The equation can also contain any \R
    functions that accepts vector arguments and returns vectors of the
    same length. It can also include functions of input \code{x} that
    return a scalar or a \code{dist} vector.}
  \item{terms}{How shared and total components are found. For vectors
    \code{x} and \code{y} the  \code{"quadratic"} terms are \code{J = sum(x*y)},
    \code{A = sum(x^2)}, \code{B = sum(y^2)}, and \code{"minimum"} terms
    are \code{J = sum(pmin(x,y))}, \code{A = sum(x)} and \code{B = sum(y)}, 
    and \code{"binary"} terms are either of these after transforming
    data into binary form (shared number of species, and number of
    species for each row). }
  \item{abcd}{Use 2x2 contingency table notation for binary data:
    \eqn{a} is the number of shared species, \eqn{b} and \eqn{c} are the
    numbers of species occurring only one of the sites but not in both,
    and \eqn{d} is the number of species that occur on neither of the sites.}
  \item{alphagamma}{Use beta diversity notation with terms
    \code{alpha} for average alpha diversity for compared sites,
    \code{gamma} for diversity in pooled sites, and \code{delta} for the
    absolute value of difference of average \code{alpha} and alpha
    diversities of compared sites. Terms \code{A} and
    \code{B} refer to alpha diversities of compared sites.}
  \item{name}{The name you want to use for your index. The default is to
    combine the \code{method} equation and \code{terms} argument.}
  \item{maxdist}{Theoretical maximum of the dissimilarity, or \code{NA}
    if index is open and has no absolute maximum. This is not a necessary
    argument, but only used in some \pkg{vegan} functions, and if you are
    not certain about the maximum, it is better not supply any value.}
}
\details{
  Most popular dissimilarity measures in ecology can be expressed with
  the help of terms \code{J}, \code{A} and \code{B}, and some also involve
  matrix dimensions \code{N} and \code{P}. Some examples you can define in
  \code{designdist} are:
  \tabular{lll}{
    \code{A+B-2*J} \tab \code{"quadratic"} \tab squared Euclidean \cr
    \code{A+B-2*J} \tab \code{"minimum"} \tab Manhattan \cr
    \code{(A+B-2*J)/(A+B)} \tab \code{"minimum"} \tab Bray-Curtis \cr
    \code{(A+B-2*J)/(A+B)} \tab \code{"binary"} \tab
    \enc{Sørensen}{Sorensen} \cr 
    \code{(A+B-2*J)/(A+B-J)} \tab \code{"binary"} \tab Jaccard \cr
    \code{(A+B-2*J)/(A+B-J)} \tab \code{"minimum"} \tab
    \enc{Ružička}{Ruzicka} \cr
    \code{(A+B-2*J)/(A+B-J)} \tab \code{"quadratic"} \tab
    (dis)similarity ratio \cr
    \code{1-J/sqrt(A*B)} \tab \code{"binary"} \tab Ochiai \cr
    \code{1-J/sqrt(A*B)} \tab \code{"quadratic"} \tab cosine
    complement \cr
    \code{1-phyper(J-1, A, P-A, B)} \tab \code{"binary"} \tab Raup-Crick (but see \code{\link{raupcrick}})
  }
  The function \code{designdist} can implement most dissimilarity
  indices in \code{\link{vegdist}} or elsewhere, and it can also be
  used to implement many other indices, amongst them, most of those
  described in Legendre & Legendre (2012). It can also be used to
  implement all indices of beta diversity described in Koleff et
  al. (2003), but there also is a specific function
  \code{\link{betadiver}} for the purpose.

  If you want to implement binary dissimilarities based on the 2x2
  contingency table notation, you can set \code{abcd = TRUE}. In this
  notation \code{a = J}, \code{b = A-J}, \code{c = B-J}, \code{d = P-A-B+J}. 
  This notation is often used instead of the more more
  tangible default notation for reasons that are opaque to me.

  With \code{alphagamma = TRUE} it is possible to use beta diversity
  notation with terms \code{alpha} for average alpha diversity and
  \code{gamma} for gamma diversity in two compared sites. The terms
  are calculated as \code{alpha = (A+B)/2}, \code{gamma = A+B-J} and
  \code{delta = abs(A-B)/2}.  Terms \code{A} and \code{B} are also
  available and give the alpha diversities of the individual compared
  sites.  The beta diversity terms may make sense only for binary
  terms (so that diversities are expressed in numbers of species), but
  they are calculated for quadratic and minimum terms as well (with a
  warning).

  Function \code{chaodist} is similar to \code{designgist}, but uses
  terms \code{U} and \code{V} of Chao et al. (2005). These terms are
  supposed to take into account the effects of unseen species. Both
  \code{U} and \code{V} are scaled to range \eqn{0 \dots 1}. They take
  the place of \code{A} and \code{B} and the product \code{U*V} is used
  in the place of \code{J} of \code{designdist}.  Function
  \code{chaodist} can implement any commonly used Chao et al. (2005)
  style dissimilarity:
  \tabular{ll}{
  \code{1 - 2*U*V/(U+V)} \tab \enc{Sørensen}{Sorensen} type \cr
  \code{1 - U*V/(U+V-U*V)} \tab Jaccard type \cr
  \code{1 - sqrt(U*V)} \tab Ochiai type \cr
  \code{(pmin(U,V) - U*V)/pmin(U,V)} \tab Simpson type
  }
  Function \code{\link{vegdist}} implements Jaccard-type Chao distance,
  and its documentation contains more complete discussion on the
  calculation of the terms.

}

\value{
  \code{designdist} returns an object of class \code{\link{dist}}.
}
\references{

  Chao, A., Chazdon, R. L., Colwell, R. K. and Shen, T. (2005) A new
  statistical approach for assessing similarity of species composition
  with incidence and abundance data. \emph{Ecology Letters} \strong{8},
  148--159.

  Koleff, P., Gaston, K.J. and Lennon, J.J. (2003) Measuring beta
  diversity for presence--absence data. \emph{J. Animal Ecol.}
  \strong{72}, 367--382. 
  
  Legendre, P. and Legendre, L. (2012) \emph{Numerical Ecology}. 3rd
  English ed. Elsevier
  }
\author{ Jari Oksanen }
\note{
  \code{designdist} does not use compiled code, but it is based on
  vectorized \R{} code. The \code{designdist} function can be much
  faster than \code{\link{vegdist}}, although the latter uses compiled
  code. However, \code{designdist} cannot skip missing values and uses
  much more memory during calculations.

  The use of sum terms can be numerically unstable. In particularly,
  when these terms are large, the precision may be lost. The risk is
  large when the number of columns is high, and particularly large with
  quadratic terms. For precise calculations it is better to use
  functions like \code{\link{dist}} and \code{\link{vegdist}} which are
  more robust against numerical problems.
}

\seealso{ \code{\link{vegdist}}, \code{\link{betadiver}}, \code{\link{dist}},
  \code{\link{raupcrick}}.}
\examples{
data(BCI)
## Five ways of calculating the same Sørensen dissimilarity
d0 <- vegdist(BCI, "bray", binary = TRUE)
d1 <- designdist(BCI, "(A+B-2*J)/(A+B)")
d2 <- designdist(BCI, "(b+c)/(2*a+b+c)", abcd = TRUE)
d3 <- designdist(BCI, "gamma/alpha - 1", alphagamma = TRUE)
d4 <- designdist(BCI, "dist(x, 'manhattan')/(A+B)")
## Zero-adjusted Bray-Curtis of Clarke et al. (J Exp Marine Biol & Ecol
## 330:55-80; 2006)
dbr0 <- designdist(BCI, "(A+B-2*J)/(A+B+2*min(x[x>0]))", terms = "minimum")
## Arrhenius dissimilarity: the value of z in the species-area model
## S = c*A^z when combining two sites of equal areas, where S is the
## number of species, A is the area, and c and z are model parameters.
## The A below is not the area (which cancels out), but number of
## species in one of the sites, as defined in designdist().
dis <- designdist(BCI, "(log(A+B-J)-log(A+B)+log(2))/log(2)")
## This can be used in clustering or ordination...
ordiplot(cmdscale(dis))
## ... or in analysing beta diversity (without gradients)
summary(dis)
  }

\keyword{multivariate }

