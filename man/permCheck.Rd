\name{permCheck}
\alias{permCheck}
\alias{numPerms}
\alias{print.permCheck}
\alias{print.summary.permCheck}
\alias{summary.permCheck}
\alias{getNumObs}
\alias{getNumObs.default}
\alias{getNumObs.integer}
\alias{getNumObs.numeric}

\title{Number of possible permutations and checking of permutation schemes}
\description{
  \code{permCheck} provides checking of permutation schemes for
  validity. \code{numPerms} calculates the maximum number of
  permutations possible under the current permutation
  scheme. \code{getNumObs} is a utility function to return the number of
  observations for a range of R and ordination objects.
}
\usage{
permCheck(object, control = permControl())

\method{summary}{permCheck}(object, \dots)

numPerms(object, control = permControl())

getNumObs(object, \dots)

\method{getNumObs}{default}(object, \dots)

\method{getNumObs}{numeric}(object, \dots)

\method{getNumObs}{integer}(object, \dots)
}

\arguments{
  \item{object}{an R object. Specifically, any object handled by
    \code{\link{scores}}, data frames, matrices, and numeric and
    integer  vectors. For \code{\link{summary.permCheck}} an object of
    class \code{"permCheck"}.}
  \item{control}{a list of control values describing properties of the
    permutation design, as returned by a call to
    \code{\link{permControl}}.}
  \item{\dots}{arguments to other methods.}
}
\details{
  \code{permCheck} and \code{numPerms} are utility functions for working
  with the new permutation schemes available in
  \code{\link{permuted.index2}}.

  \code{permCheck} is used to check the current permutation schemes
  against the object to which it will be applied. It calculates the
  maximum number of possible permutations for the number of observations
  in \code{object} and the permutation scheme described by
  \code{control}. The returned object contains component \code{control},
  an object of class \code{"permControl"} suitably modified if
  \code{permCheck} identifies a problem.

  The main problem is requesting more permutations than possible with
  the number of observations and the permutation design. In such cases,
  \code{nperm} is reduced to equal the number of possible permutations,
  and complete enumeration of all permutations is turned on
  (\code{control$complete} is set to \code{TRUE}). 

  Alternatively, if the number of possible permutations is low, and less
  than \code{control$minperm}, it is better to enumerate all possible
  permutations, and as such complete enumeration of all permutations is
  turned  on (\code{control$complete} is set to \code{TRUE}).
}
\value{
  For \code{permCheck} a list containing the maximum number of
  permutations possible and an object of class
  \code{"\link{permControl}"}.

  For \code{numPerms}, the (numeric) number of possible permutations.

  For \code{getNumObs}, the (numeric) number of observations in
  \code{object}.
}
%\references{
%}
\note{
  Currently, complete enumeration is not implemented in
  \code{\link{permuted.index2}}. \code{permCheck} sets complete
  enumeration in the \code{control} component of the returned object
  but this is not acted upon in the permutations.

  \code{permCheck} does reduce the \code{nperm} and \code{maxperm}
  components of the returned object to the maximum number of possible
  permutations, and \code{\link{permuted.index2}} obeys these
  directives.
}
\author{Gavin Simpson}
\seealso{\code{\link{permuted.index2}} and \code{\link{permControl}}.}

\examples{
## use example data from ?pyrifos
example(pyrifos)

## Demonstrate the maximum number of permutations for the pyrifos data
## under a series of permutation schemes

## no restrictions - lots of perms
(check1 <- permCheck(pyrifos, control = permControl(type = "free")))
summary(check1)

## no strata but data are series with no mirroring, so 132 permutations
permCheck(pyrifos, control = permControl(type = "series",
                   mirror = FALSE))

## no strata but data are series with mirroring, so 264 permutations
permCheck(pyrifos, control = permControl(type = "series",
                   mirror = TRUE))

## unrestricted within strata
permCheck(pyrifos, control = permControl(strata = ditch,
                   type = "free"))

## time series within strata, no mirroring
permCheck(pyrifos, control = permControl(strata = ditch,
                   type = "series", mirror = FALSE))

## time series within strata, with mirroring
permCheck(pyrifos, control = permControl(strata = ditch,
                   type = "series", mirror = TRUE))

## time series within strata, no mirroring, same permutation within strata
permCheck(pyrifos, control = permControl(strata = ditch,
                   type = "series", constant = TRUE))

## time series within strata, with mirroring, same permutation within strata
permCheck(pyrifos, control = permControl(strata = ditch,
                   type = "series", mirror = TRUE, constant = TRUE))

## permute strata
permCheck(pyrifos, permControl(strata = ditch, type = "strata"))

## this should also also for arbitrary vectors
vec1 <- permCheck(1:100)
vec2 <- permCheck(1:100, permControl())
all.equal(vec1, vec2)
vec3 <- permCheck(1:100, permControl(type = "series"))
all.equal(100, vec3$n)
vec4 <- permCheck(1:100, permControl(type = "series", mirror = TRUE))
all.equal(vec4$n, 200)

}
\keyword{ utilities }
\keyword{ design }

