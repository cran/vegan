\name{vegan-internal}
\alias{ordiParseFormula}
\alias{permuted.index}
\alias{centroids.cca}
\alias{ordiTerminfo}
\alias{ordispantree}

\title{Internal vegan functions}

\description{
  Internal vegan functions.
}
\usage{
ordiParseFormula(formula, data, xlev = NULL)
ordiTerminfo(d, data)
centroids.cca(x, mf, wt)
permuted.index(n, strata)
ordispantree(ord, tree, display = "sites", ...)
}

\details{
  These are not to be called by the user. Function \code{ordispantree}
  is deprecated: use \code{\link{lines.spantree}} instead.
}

\keyword{internal }

