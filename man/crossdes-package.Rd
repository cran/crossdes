\name{crossdes-package}
\alias{crossdes-package}
\alias{crossdes}
\docType{package}
\title{
  Design and Analysis of dose-finding studies
}
\description{
  The crossdes package provides functions for the construction of of
  carryover balanced crossover designs. In addition contains functions
  to check given designs for balance.
}
\details{
\tabular{ll}{
Package: \tab crossdes\cr
Type: \tab Package\cr
Version: \tab 1.1-1\cr
Date: \tab 2013-03-18\cr
License: \tab GPL-2
}
The main functions are:\cr
\code{\link{get.plan}}: Menu-driven selection of crossover designs\cr
\code{\link{allcombs}}: Construct crossover design with all possible
treatment orders\cr
\code{\link{williams}}: Construct a Williams design\cr
\code{\link{williams.BIB}}: Construct a crossover design based on a
combination of balanced incomplete block designs and Williams designs.\cr
\code{\link{des.MOLS}}: Construct crossover design based on mutually
orthogonal Latin Squares\cr
\code{\link{balmin.RMD}}: Construct balanced minimal repeated
measurements crossover design\cr
\code{\link{isCbalanced}}: Check whether a crossover design is balanced
for first order carryover effects\cr
\code{\link{isGYD}}: Check whether a crossover design is balanced\cr
\cr
}
\author{
Martin Oliver Sailer
}
\references{
  Earlier version of the package:\cr
  Sailer, O. (2005): crossdes: A package for design and randomization in crossover studies. Rnews 5/2, 24-27.  

  Overview on cross-over designs:\cr
  Jones, B. and Kenward, M.G. (1989): Design and Analysis of Cross-Over Trials. Chapman and Hall, London.
  
  Wakeling, I.N. and MacFie, H.J.H. (1995): Designing consumer trials balanced for first and higher orders of
  carry-over effect when only a subset of k samples from t may be tested. Food Quality and Preference 6, 299-308. 
}
\keyword{ package }
