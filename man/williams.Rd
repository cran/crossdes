\name{williams}
\alias{williams}
\title{Construction of Williams Designs }
\description{
  The function constructs williams designs. Williams designs are row-column designs. They are used if 
  each of the treatments in the study is given to each of the subjects. If the number of 
  treatments to be tested is even, the design is a latin square, otherwise it consists of two latin squares.
}
\usage{
williams(trt)
}
\arguments{
  \item{trt}{ An integer > 1, giving the number of treatments in the design. }
}
\details{
  The resulting design is a (generalized) latin square that is also balanced for first order carryover effects. 
  Carryover balance is achieved with very few subjects.
  In the experimental design the treatments are numbered 1,\ldots,\eqn{trt}{trt}. The entry 
  \eqn{(i,j)}{(i,j)} of the design corresponds 
  to the treatment the \eqn{i}{i}-th subject gets in the \eqn{j}{j}-th period.
}
\value{
  A matrix representing the experimental design.
}
\references{ 
Wakeling, I.N. and MacFie, H.J.H. (1995): Designing consumer trials balanced for first and higher orders of
carry-over effect when only a subset of k samples from t may be tested. Food Quality and Preference 6, 299-308. 

Williams, E. J. (1949): Experimental designs balanced for the estimation of residual effects
of treatments. Australian Journal of Scientific Research, Ser. A 2, 149-168. 
}

\author{ Oliver Sailer \email{sailer@statistik.uni-dortmund.de}}

\seealso{ \code{\link{get.plan}} }
\examples{
williams(3)
williams(10)
}

\keyword{ design }% at least one, from doc/KEYWORDS
