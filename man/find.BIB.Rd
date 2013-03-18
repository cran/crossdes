\name{find.BIB}
\alias{find.BIB}
\title{Generation of Balanced Incomplete Block Designs Using the Package AlgDesign}
\description{
  The function \code{optBlock} of the library \code{AlgDesign} is used to search for balanced incomplete block designs
  (BIBDs). The design is assigned to a matrix where rows represent blocks (subjects) and 
  columns represent periods.  
}
\usage{
find.BIB(trt, b, k, iter = 30)
}
\arguments{
  \item{trt}{ An integer > 1 giving the number of treatments of the design. }
  \item{b}{ An integer > 1 giving the number of rows (subjects) of the design. }
  \item{k}{ An integer > 1 giving the number of columns (periods) of the design. }
  \item{iter}{ The number of iterations of the function \code{optBlock} } 
}
\details{
  The function \code{optBlock} tries to find a D-optimal block design for the specified parameters.
  The resulting design need not be a BIBD. 
  The necessary conditions for the existence are that \eqn{\frac{bk}{trt}}{bk/trt} and 
   \eqn{\frac{bk(k-1)}{trt(trt-1)}}{bk(k-1)/(trt(trt-1))} positive integers.
  They are NOT checked automatically.
  Even if they are fulfilled, there need not be a BIBD.  
  If no BIBD is found, the function is iterated.
  If no BIBD is found after \code{iter} iterations, the search is terminated.
  The resulting design should be checked by the user applying \code{isGYD}. 
}
\value{
  A matrix that represents the experimental design.
}
\references{ 
  Wheeler, R.E. (2004). optBlock. AlgDesign. The R project for statistical computing http://www.r-project.org/ 
}
\author{Oliver Sailer }
\note{ As indicated above, the returned design is not necessarily a BIBD design.}

\seealso{ \code{\link{get.plan}}, \code{\link[AlgDesign]{optBlock}}}
\examples{
find.BIB(10,30,4)       # BIBD
find.BIB(3,3,3)         # BIBD 
find.BIB(5,5,3, 100)    # There is no BIBD for these parameters   
# isGYD(find.BIB(5,5,3, 100))
}

\keyword{ design }% at least one, from doc/KEYWORDS
