\name{all.combin}
\alias{all.combin}
\title{Balanced Row-Column Design with all Possible Treatment Orders.}
\description{
  The function constructs a row-column design with subjects as rows and periods as columns. Each subject
  gets each treatment at most once. All possible treatment orders are assigned to the subjects. }
\usage{
all.combin(trt, k)
}
\arguments{
  \item{trt}{ An integer > 1. Number of treatments (products) to be tested. }
  \item{k}{ An integer \eqn{\leq \, trt}{<= trt}. Number of periods for each subject. }
}
\details{
  The design is a carryover balanced generalized Youden design that is uniform on the columns.
  The treatments are numbered 1,\ldots,\eqn{trt}{trt}. The entry \eqn{(i,j)}{(i,j)} 
  of the design corresponds to the treatment the \eqn{i}{i}-th subject gets in the \eqn{j}{j}-th period.
}
\value{
  A matrix with \eqn{\frac{trt!}{(trt-k)!}}{trt!/(trt-k)!} rows and \eqn{k}{k} columns representing the experimental design.
}
\references{ 
Patterson, H.D. (1952): The construction of balanced designs for experiments involving sequences of
treatments. Biometrika 39, 32-48. 

Wakeling, I.N. and MacFie, H.J.H. (1995): Designing consumer trials balanced for first and higher orders of
carry-over effect when only a subset of k samples from t may be tested. Food Quality and Preference 6, 299-308. }
\author{ Oliver Sailer \email{sailer@statistik.uni-dortmund.de} }
\note{ Requires the package \code{gtools}. }


\seealso{ \code{\link{get.plan}} }
\examples{
# Design for 4 treatments assigned in 3 periods.
# All possible treatment orders occur.
all.combin(4,3) 
}

\keyword{ design }
