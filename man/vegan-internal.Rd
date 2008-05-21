\name{vegan-internal}
\alias{ordiGetData}
\alias{ordiParseFormula}
\alias{permuted.index}
\alias{centroids.cca}
\alias{ordiTerminfo}
\alias{pasteCall}
\alias{ordiArrowMul}
\alias{ordiArgAbsorber}
\title{Internal vegan functions}

\description{
  Internal vegan functions.
}
\usage{
ordiGetData(call, env)
ordiParseFormula(formula, data, xlev = NULL)
ordiTerminfo(d, data)
ordiArrowMul(x, at = c(0,0), fill = 0.75)
ordiArgAbsorber(..., shrink, origin, scaling, triangular, FUN)
centroids.cca(x, mf, wt)
permuted.index(n, strata)
pasteCall(call, prefix = "Call:")
}

\details{
  These are not to be called by the user. Function \code{ordispantree}
  is deprecated: use \code{\link{lines.spantree}} instead.
}

\keyword{internal }
