\encoding{UTF-8}
\name{multipart}
\alias{multipart}
\alias{print.multipart}
\title{Multiplicative Diversity Partitioning}
\description{
In multiplicative diversity partitioning, mean values of alpha diversity at lower levels of a sampling 
hierarchy are compared to the total diversity in the entire data set or the pooled samples (gamma diversity). 
}
\usage{
multipart(formula, data, index=c("renyi", "tsallis"), scales = 1,
    global = FALSE, relative = FALSE, nsimul=99, ...)
\method{print}{multipart}(x, ...)
}
\arguments{
  \item{formula}{A two sided model formula in the form \code{y ~ x}, where \code{y} 
    is the community data matrix with samples as rows and species as column. Right 
    hand side (\code{x}) must contain factors referring to levels of sampling hierarchy, 
    terms from right to left will be treated as nested (first column is the lowest, 
    last is the highest level). These variables must be factors in order to unambiguous 
    handling. Interaction terms are not allowed.}
  \item{data}{A data frame where to look for variables defined in the right hand side 
    of \code{formula}. If missing, variables are looked in the global environment.}
  \item{index}{Character, the entropy index to be calculated (see Details).}
  \item{relative}{Logical, if \code{TRUE} then beta diversity is
    standardized by its maximum (see Details).}
  \item{scales}{Numeric, of length 1, the order of the generalized diversity index 
    to be used.}
  \item{global}{Logical, indicates the calculation of beta diversity values, see Details.}
  \item{nsimul}{Number of permutation to use if \code{matr} is not of class 'permat'.
    If \code{nsimul = 0}, only the \code{FUN} argument is evaluated. It is thus possible
    to reuse the statistic values without using a null model.}
  \item{x}{An object to print.}
  \item{\dots}{Other arguments passed to \code{\link{oecosimu}}, i.e. 
    \code{method}, \code{thin} or \code{burnin}.}
}
\details{
Multiplicative diversity partitioning is based on Whittaker's (1972) ideas, that has 
recently been generalised to one parametric diversity families (i.e. \enc{Rényi}{Renyi} 
and Tsallis) by Jost (2006, 2007). Jost recommends to use the numbers equivalents 
(Hill numbers), instead of pure diversities, and proofs, that this satisfies the 
multiplicative partitioning requirements.

The current implementation of \code{multipart} calculates Hill numbers based on the 
functions \code{\link{renyi}} and \code{\link{tsallis}} (provided as \code{index} argument). 
If values for more than one \code{scales} are desired, it should be done in separate 
runs, because it adds extra dimensionality to the implementation, which has not been resolved 
efficiently.

Alpha diversities are then the averages of these Hill numbers for each hierarchy levels, 
the global gamma diversity is the alpha value calculated for the highest hierarchy level. 
When \code{global = TRUE}, beta is calculated relative to the global gamma value:
\deqn{\beta_i = \gamma / \alpha_{i}}{beta_i = gamma / alpha_i}
when \code{global = FALSE}, beta is calculated relative to local gamma values (local gamma
means the diversity calculated for a particular cluster based on the pooled abundance vector):
\deqn{\beta_ij = \alpha_{(i+1)j} / mean(\alpha_{ij})}{beta_ij = alpha_(i+1)j / mean(alpha_i)}
where \eqn{j} is a particular cluster at hierarchy level \eqn{i}. Then beta diversity value for
level \eqn{i} is the mean of the beta values of the clusters at that level,
\eqn{\beta_{i} = mean(\beta_{ij})}.

If \code{relative = TRUE}, the respective beta diversity values are
standardized by their maximum possible values (\eqn{mean(\beta_{ij}) / \beta_{max,ij}})
given as \eqn{\beta_{max,ij} = n_{j}} (the number of lower level units in a given cluster \eqn{j}).

The expected diversity components are calculated \code{nsimul} times by individual based 
randomisation of the community data matrix. This is done by the \code{"r2dtable"} method
in \code{\link{oecosimu}} by default.
}
\value{
An object of class 'multipart' with same structure as 'oecosimu' objects.
}
\references{
Jost, L. (2006). Entropy and diversity.
\emph{Oikos}, \bold{113}, 363--375.

Jost, L. (2007). Partitioning diversity into independent alpha and beta components.
\emph{Ecology}, \bold{88}, 2427--2439.

Whittaker, R. (1972). Evolution and measurement of species diversity.
\emph{Taxon}, \bold{21}, 213--251.
}
\author{\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}}
\seealso{See \code{\link{adipart}} for additive diversity partitioning,
  \code{\link{hiersimu}} for hierarchical null model testing
  and \code{\link{oecosimu}} for permutation settings and calculating \eqn{p}-values.}
\examples{
data(mite)
data(mite.xy)
data(mite.env)
## Function to get equal area partitions of the mite data
cutter <- function (x, cut = seq(0, 10, by = 2.5)) {
    out <- rep(1, length(x))
    for (i in 2:(length(cut) - 1))
        out[which(x > cut[i] & x <= cut[(i + 1)])] <- i
    return(as.factor(out))}
## The hierarchy of sample aggregation
levsm <- data.frame(
    l1=as.factor(1:nrow(mite)),
    l2=cutter(mite.xy$y, cut = seq(0, 10, by = 2.5)),
    l3=cutter(mite.xy$y, cut = seq(0, 10, by = 5)),
    l4=cutter(mite.xy$y, cut = seq(0, 10, by = 10)))
## Multiplicative diversity partitioning
multipart(mite ~ ., levsm, index="renyi", scales=1, nsimul=25)
multipart(mite ~ ., levsm, index="renyi", scales=1, nsimul=25, relative=TRUE)
multipart(mite ~ ., levsm, index="renyi", scales=1, nsimul=25, global=TRUE)
}
\keyword{multivariate}