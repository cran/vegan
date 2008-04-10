\name{permuted.index2}
\alias{permuted.index2}
\alias{permControl}
\alias{print.permControl}

\title{Unrestricted and restricted permutations}
\description{
  Unrestricted and restricted permutation designs for time series,
  line transects, spatial grids and blocking factors.
}
\usage{
permuted.index2(n, control = permControl())

permControl(strata = NULL, nperm = 199, complete = FALSE,
            type = c("free", "series", "grid", "strata"),
            maxperm = 9999, minperm = 99,
            mirror = FALSE, constant = FALSE,
            ncol = NULL, nrow = NULL,
            all.perms = NULL)
}

\arguments{
  \item{n}{numeric; the length of the returned vector of permuted
    values. Usually the number of observations under consideration.}
  \item{control}{a list of control values describing properties of the
    permutation design, as returned by a call to \code{permControl}.}
  \item{strata}{An integer vector or factor specifying the strata for
    permutation. If supplied, observations are permuted only within the
    specified strata.}
  \item{nperm}{the number of permutations.}
  \item{complete}{logical; should complete enumeration of all
    permutations be performed?}
  \item{type}{the type of permutations required. One of \code{"free"},
    \code{"series"}, \code{"grid"} or \code{"strata"}. See Details.}
  \item{maxperm}{the maximum number of permutations to
    perform. Currently unused.}
  \item{minperm}{the lower limit to the number of possible permutations
    at which complete enumeration is performed. See argument
    \code{complete} and Details, below.}
  \item{mirror}{logical; should mirroring of sequences be allowed?}
  \item{constant}{logical; should the same permutation be used within
    each level of strata? If \code{FALSE} a separate, possibly restricted,
    permutation is produced for each level of \code{strata}.}
  \item{ncol, nrow}{numeric; the number of columns and rows of samples
    in the spatial grid respectiavly.}
  \item{all.perms}{an object of class \code{allPerms}, the result of a
    call to \code{\link{allPerms}}.}
}
\details{
  \code{permuted.index2} can generate permutations for a wide range of
  restricted permutation schemes. A small selection of the available
  combinations of options is provided in the Examples section below.

  Argument \code{mirror} determines whether grid or series permutations
  can be mirrored. Consider the sequence 1,2,3,4. The relationship
  between consecutive observations is preserved if we reverse the
  sequence to 4,3,2,1. If there is no inherent direction in your
  experimental design, mirrored permutations can be considered
  part of the Null model, and as such increase the number of possible
  permutations. The default is to not use mirroring so you must
  explicitly turn this on using \code{mirror = TRUE} in
  \code{permControl}.

  To permute \code{strata} rather than the observations within the
  levels of \code{strata}, use \code{type = "strata"}. However, note
  that the number of observations within each level of strata
  \strong{must} be equal! 

  For some experiments, such as BACI designs, one might wish to use the
  same permutation within each level of strata. This is controlled by
  argument \code{constant}. If \code{constant = TRUE} then the same
  permutation will be generated for each level of \code{strata}. The
  default is \code{constant = FALSE}.
}
\value{
  For \code{permuted.index2} a vector of length \code{n} containing a
  permutation of the observations 1, \ldots, n using the permutation
  scheme described by argument \code{control}.
  
  For \code{permControl} a list with components for each of the possible
  arguments.
}
\note{
  \code{permuted.index2} is currently used in one Vegan function;
  \code{\link{permDisper}}. Over time, the other functions that
  currently use the older \code{\link{permuted.index}} will be updated
  to use \code{permuted.index2}.
}
%\references{
%}
\author{Gavin Simpson}
\seealso{\code{\link{permCheck}}, a utility function for checking
  permutation scheme described by \code{\link{permControl}}.}

\examples{
set.seed(1234)

## unrestricted permutations
permuted.index2(20)

## observations represent a time series of line transect
permuted.index2(20, control = permControl(type = "series"))

## observations represent a time series of line transect
## but with mirroring allowed
permuted.index2(20, control = permControl(type = "series", mirror = TRUE))

## observations represent a spatial grid
perms <- permuted.index2(20, permControl(type = "grid",
                                         ncol = 4, nrow = 5))
## view the permutation as a grid
matrix(matrix(1:20, nrow = 5, ncol = 4)[perms], ncol = 4, nrow = 5)

## random permutations in presence of strata
block <- gl(4, 5)
permuted.index2(20, permControl(strata = block, type = "free"))
## as above but same random permutation within strata
permuted.index2(20, permControl(strata = block, type = "free",
                                constant = TRUE))

## time series within each level of block
permuted.index2(20, permControl(strata = block, type = "series"))
## as above, but  with same permutation for each level
permuted.index2(20, permControl(strata = block, type = "series",
                                constant = TRUE))

## spatial grids within each level of block
permuted.index2(100, permControl(strata = block, type = "grid",
                                 ncol = 5, nrow = 5))
## as above, but with same permutation for each level
permuted.index2(100, permControl(strata = block, type = "grid",
                                 ncol = 5, nrow = 5, constant = TRUE))

## permuting levels of block instead of observations
permuted.index2(20, permControl(strata = block, type = "strata"))

}
\keyword{ htest }
\keyword{ design }
