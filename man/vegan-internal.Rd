\name{vegan-internal}
\alias{ordiParseFormula}
\alias{permuted.index}
\alias{centroids.cca}
\alias{spider.cca}
\alias{ordiTerminfo}

\title{Internal vegan functions}

\description{
  Internal vegan functions.
}
\usage{
ordiParseFormula(formula, data, xlev = NULL)
ordiTerminfo(d, data)
centroids.cca(x, mf, wt)
permuted.index(n, strata)
spider.cca(x, ...)
}

\details{
  These are not to be called by the user. Function \code{spider.cca} was
  replaced with \code{\link{ordispider}} and will be removed in the
  future. 
}

\keyword{internal }

