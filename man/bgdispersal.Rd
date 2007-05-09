\encoding{UTF-8}
\name{bgdispersal}
\alias{bgdispersal}

\title{ Coefficients of Biogeographical Dispersal Direction }

\description{ This function computes coefficients of dispersal direction
between geographically connected areas, as defined by Legendre and
Legendre (1984), and also described in Legendre and Legendre (1998,
section 13.3.4). }

\usage{
bgdispersal(mat, PAonly = FALSE)
}

\arguments{
\item{mat}{ Data frame or matrix containing a community composition data table
(species presence-absence or abundance data) }
\item{PAonly}{ \code{FALSE} if the four types of coefficients, DD1 to
  DD4, are requested; \code{TRUE} if DD1 and DD2 only are sought (see Details). }
}

\details{ The signs of the DD coefficients indicate the direction of
dispersal, provided that the asymmetry is significant. A positive sign
indicates dispersal from the first (row in DD tables) to the second
region (column); a negative sign indicates the opposite.  Coefficients
DD1 and DD2 are computed from presence-absence data. The function
converts abundance data to presence-absence if necessary. Coefficients
DD3 and DD4 are computed on abundance data if available.  A McNemar
test of asymmetry is computed from the presence-absence data to test the
hypothesis of a significant asymmetry between the two areas under
comparison.

In the input data table, the rows are sites or areas, the columns are
taxa. Most often, the taxa are species, but the coefficients can be
computed from genera or families as well.  DD1 and DD2 only are computed
for presence-absence data. The four types of coefficients are computed
for quantitative data, which are converted to presence-absence for the
computation of DD1 and DD2.  \code{PAonly = FALSE} indicates that the four types
of coefficients are requested. \code{PAonly = TRUE} if DD1 and DD2 only are
sought. }

\value{
Function \code{dispersion} returns a list containing the following matrices:

\item{ DD1 }{ \eqn{DD1[j,k] = (a * (b - c))/((a + b + c)^2)} }

\item{ DD2 }{ \eqn{DD2[j,k] = (2*a * (b - c))/((2*a + b + c) * (a + b +
c))} where \eqn{a}, \eqn{b}, and \eqn{c} have the same meaning as in the
same meaning as in the computation of binary similarity coefficients. }

\item{ DD3 }{ \eqn{DD3[j,k] = W*(A-B) / ((A+B-W)^2)} }

\item{ DD4 }{ \eqn{DD4[j,k] = 2*W*(A-B) / ((A+B)*(A+B-W))}
  where \code{W = sum(pmin(vector1, vector2))}, \code{A = sum(vector1)}, \code{B =
  sum(vector2)} }

\item{ McNemar }{ McNemar chi-square statistic of asymmetry with
correction for continuity (Siegel and Castellan 1988): \eqn{(abs(b - c) -
1)^2 / (b + c)} }

\item{ prob.McNemar }{ probabilities associated with McNemar statistics,
  chi-square test. H0: no asymmetry in \eqn{(b-c)}. }
}

\references{ 
  Legendre, P. and V. Legendre. 1984. Postglacial dispersal of
  freshwater fishes in the Québec
  peninsula. Can. J. Fish. Aquat. Sci. 41: 1781-1802.
  
  Legendre, P. and L. Legendre. 1998. Numerical ecology, 2nd English
  edition. Elsevier Science BV, Amsterdam.
  
  Siegel, S. and N. J. Castellan, Jr. 1988. Nonparametric statistics for
  the behavioral sciences. 2nd edition. McGraw-Hill, New York.
}

\author{ Pierre Legendre, Departement de Sciences Biologiques, Universite de Montreal }

\examples{
mat <- matrix(c(32,15,14,10,70,30,100,4,10,30,25,0,18,0,40,
  0,0,20,0,0,0,0,4,0,30,20,0,0,0,0,25,74,42,1,45,89,5,16,16,20),
  4, 10, byrow=TRUE)
bgdispersal(mat)
}

\keyword{ multivariate }
\keyword{ nonparametric }

