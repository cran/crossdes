\name{rand.design.RC}
\alias{rand.design.RC}
\title{Simulation Study to Asses the Validity of a Randomization Procedure}
\description{
  The function performs a simulation study to assess whether randomization of treatment labels and rows (subjects) 
  validates the row-column model
  for a given design. The results are stored to a file. 
}
\usage{
rand.design.RC(design, dat, tau1, rho, n, where)
}
\arguments{
  \item{design}{ A matrix with \eqn{b}{b} rows and \eqn{k}{k} columns representing the experimental design. 
  Treatments are numbered 1,\ldots,\eqn{trt}{trt}. }
  \item{dat}{ A numerical vector with \eqn{bk}{bk} elements giving the data to be used for the simulation study.
              The first \eqn{k}{k} values of \code{dat} correspond to the first row of the design,
              the next \eqn{k}{k} values correspond to the second row etc. }
  \item{tau1}{ The value of the main effect of treatment 1. }
  \item{rho}{ The value that is used for the carryover (residual) effects of treatments 1 and 2. }
  \item{n}{ The number of permutations in the simulation study. }
  \item{where}{ Path that gives the location of the simulation results.}
}
\details{
  The simulation study proceeds as follows:
  For every iteration, treatment labels and rows of the design are randomized. Then the elementary contrast 
  \eqn{tau_1 - tau_{trt}}{tau_1 - tau_trt} is estimated and the estimate of the variance of this contrast is computed.
  These computations are done for each of six situations:
  1) There are no direct or residual effects of treatments.
  2) There is a direct effect of treatment 1.
  In 3) and 4), a residual effect of treatment 2 is added while 
  in 5) and 6), a residual effect of treatment 1 is added.   
  The estimates are then stored to \code{where}.
}
\value{
  There is no value returned. The results are stored in a file.
}
\references{ Bailey, R.A. and Rowley, C.A. (1985): Valid randomization. Proceedings of the Royal Society London A 410, 105-124. 

             Kunert, J. and Sailer, O. (2004): On nearly balanced designs for sensory trials. In preparation.
}
\author{Oliver Sailer \email{sailer@statistik.uni-dortmund.de} }
\note{ You need to call \code{analyze.rand} to display and interpret the results.
       \code{rand.design.RC} just performs the simulation study. }

\seealso{  \code{\link{analyze.rand}}, \code{\link{rand.design.azais}} }
\examples{
\dontrun{ 
# First create a data set to analyze:
d <- matrix(c(1:4,2:4,1,4,1:3,3,4,1,2),ncol=4)
rand.design.RC( d, rnorm(16), -1, 1, 1000, "D:\\mytest.txt" )
# Now do the analysis:
analyze.rand( "D:\\mytest.txt", fig=TRUE, ref=TRUE, 
 refval=c(0, -1, 0, -1, -.25, -1.25) ) 
}
}

\keyword{ design } % at least one, from doc/KEYWORDS
