\name{read.cep}
\alias{read.cep}

\title{Reads a CEP (Canoco) data file }
\description{
  \code{read.cep} reads a file formatted by relaxed strict CEP format
  used by \code{Canoco} software, among others.
}
\usage{
read.cep(file, maxdata=10000, positive=TRUE, trace=FALSE, force=FALSE)
}

\arguments{
  \item{file}{File name (character variable). }
  \item{maxdata}{Maximum number of non-zero entries. }
  \item{positive}{Only positive entries, like in community data.}
  \item{trace}{Work verbosely.}
  \item{force}{Run function, even if \R refuses first.}
}
\details{
  Cornell Ecology Programs (CEP) introduced several data formats
  designed for punched cards.  One of these was the `condensed strict'
  format which was adopted by popular software \code{DECORANA} and
  \code{TWINSPAN}. Later, Cajo ter Braak wrote \code{Canoco}
  based on \code{DECORANA}, where he adopted the format, but relaxed it
  somewhat (that's why I call it a `relaxed strict' format). Further, he
  introduced a more ordinary `free' format, and allowed the use of
  classical Fortran style `open' format with fixed field widths.  This
  function should be able to deal with all these \code{Canoco} formats,
  whereas it cannot read many of the traditional CEP alternatives.

  All variants of CEP formats have:
  \itemize{
    \item Two or three title cards, most importantly specifying the format (or word
    \code{FREE}) and the number of items per record (number of species
    and sites for \code{FREE} format).
    \item Data in one of three accepted formats:
    \enumerate{
      \item Condensed format: First number on the line is the site
      identifier, and it is followed by pairs (`couplets') of numbers
      identifying the species and its abundance (an integer and a floating
      point number).
      \item Open Fortran format, where the first number on the line must
      be the site number, followed by abundance values in fields of
      fixed widths. Empty fields are interpreted as zeros.
      \item `Free' format, where the numbers are interpreted as
      abundance values.  These numbers must be separated by blank space,
      and zeros must be written as zeros.
    }
    \item Species and site names, given in Fortran format \code{(10A8)}:
    Ten names per line, eight columns for each.
  }

  With option \code{positive = TRUE} the function removes all lines and
  columns with zero or negative marginal sums.  In community data
  with only positive entries, this removes empty sites and species.
  If data entries can be negative, this ruins data, and such data sets
  should be read in with option \code{positive = FALSE}.
}
\value{
  Returns a data frame, where columns are species and rows are
  sites. Column and row names are taken from the CEP file, and changed
  into unique \R names by \code{\link{make.names}} after stripping the blanks.
}
\references{ 
  Ter Braak, C.J.F. (1984--): CANOCO -- a FORTRAN program for \emph{cano}nical
  \emph{c}ommunity \emph{o}rdination by [partial] [detrended] [canonical]
  correspondence analysis, principal components analysis and redundancy
  analysis. \emph{TNO Inst. of Applied Computer Sci., Stat. Dept. Wageningen,
  The Netherlands}. 
}
\author{ Jari Oksanen }

\note{
  The function relies on smooth linking of Fortran file IO in \R
  session.  This is not guaranteed to work, and therefore the function
  is not portable.  It works at least with a lucky combination of
  \code{gcc}, \code{g77} and \code{libc} releases in \code{Linux}, like
  in Red Hat 7.1, where this function was developed.  However, it may
  not work in your system, but it
  can crash the \R session.  Therefore the default is that the function
  does not run.  If you still want to try:
  \enumerate{
    \item
    Save your session
    \item
    Run \code{read.cep()} with switch \code{force=TRUE}
    \item
    If successful, tell me.
  }
}


\examples{
## Provided that you have the file `dune.spe'
\dontrun{theclassic <- read.cep("dune.spe", force=T)}
}
\keyword{ IO }
\keyword{ file }




