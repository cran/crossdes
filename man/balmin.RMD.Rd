\name{balmin.RMD}
\alias{balmin.RMD}
\title{Function to construct the balanced minimal 
       repeated measurements designs of Afsarinejad (1983)}
\description{
  The function constructs a row-column design with subjects as rows and periods as columns. The design is incomplete, i.e.
  no subject gets all the treatments. The design is balanced for carryover effects but will in general not be
  a balanced block design.}
\usage{
balmin.RMD(trt, n, p)
}
\arguments{
  \item{trt}{ An integer >1 giving the number of treatments (products) to be tested. }
  \item{n}{ An integer >1 giving the number of subjects (assessors) in the study. }
  \item{p}{ An integer >1 giving the number of periods for each subject. }
}
\details{
  A necessary and sufficient condition for the existence of such a design is 
  that \eqn{\frac{(trt-1)}{(p-1)}}{(trt-1)/(p-1)} be a positive integer.
  In this case \eqn{n = \frac{trt(trt-1)}{(p-1)}}{n = trt(trt-1)/(p-1)}.  
  In the resulting design the treatments are numbered 1,\ldots,\eqn{trt}{trt}. 
  The entry \eqn{(i,j)}{(i,j)} 
  of the design corresponds to the treatment the \eqn{i}{i}-th subject gets in the \eqn{j}{j}-th period.
}
\value{
  A matrix with \eqn{n}{n} rows and \eqn{p}{p} columns representing the experimental design.
}

\references{ 
 Afsarinejad, K. (1983): Balanced repeated measurements designs. Biometrika 70, 199-204.
             
 Wakeling, I.N. and MacFie, H.J.H. (1995): Designing consumer trials balanced for first and higher orders of
 carry-over effect when only a subset of k samples from t may be tested. Food Quality and Preference 6, 299-308. }
\author{ Oliver Sailer \email{sailer@statistik.uni-dortmund.de}}


\seealso{ \code{\link{get.plan}} }
\examples{
balmin.RMD(10,30,4) # a balanced minimal RMD
balmin.RMD(11,22,6) # another balanced minimal RMD
}
    


\keyword{ design }
