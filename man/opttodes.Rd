\name{opttodes}
\alias{opttodes}
\title{ Generation of Balanced Incomplete Block Designs Using the Package Dopt }
\description{
  The function \code{Dopt} of the library \code{Dopt} is used to construct balanced incomplete block designs
  (BIBDs). The design is assigned to a matrix where rows represent blocks (subjects) and 
  columns represent periods.  
}
\usage{
opttodes(trt, b, k, iter = 50)
}
\arguments{
  \item{trt}{ An integer > 1 giving the number of treatments of the design. }
  \item{b}{ An integer > 1 giving the number of rows (subjects) of the design. }
  \item{k}{ An integer > 1 giving the number of columns (periods) of the design. }
  \item{iter}{ The number of iterations of the function \code{Dopt} }
}
\details{
  The function \code{Dopt} tries to find a D-optimal block design for the specified parameters.
  The resulting design need not be a BIBD. 
  The necessary conditions for the existence are that \eqn{\frac{bk}{trt}}{bk/trt} and 
   \eqn{\frac{bk(k-1)}{trt(trt-1)}}{bk(k-1)/(trt(trt-1))} positive intergers.
  They are NOT checked automatically.
  Even if they are fulfilled, there need not be a BIBD.  
  If no BIBD is found, the function is iterated.
  If no BIBD is found after \code{iter} iterations, the search is terminated.
  In any case, the resulting design is checked by the function \code{isGYD}, the result is
  displayed on the screen and the resulting design matrix is returned. 
}
\value{
  A matrix that represents the experimental design.
}

\author{Oliver Sailer \email{sailer@statistik.uni-dortmund.de}}
\note{ As indicated above, the returned design is not necessarily a BIBD design.}

\section{Warning }{ This function requires the package \code{Dopt}. 
 As of May 13 2004, CRAN states that \code{Dopt} is a package in development and that such packages may be unstable.
}

\seealso{ \code{\link{get.plan}}, \code{\link[Dopt]{Dopt}}}
\examples{
opttodes(10,30,4)       # BIBD
opttodes(3,3,2)         # BIBD, even GYD
opttodes(5,5,3, 100)    # There is no BIBD for these parameters   
}

\keyword{ design }% at least one, from doc/KEYWORDS
