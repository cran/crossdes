\name{williams.BIB}
\alias{williams.BIB}
\title{Construction of Carryover Balanced Designs Based on 
 Balanced Incomplete Block Designs}
\description{
  Patterson (1951) combined balanced incomplete block designs (BIBDs) with Williams designs to get 
  carryover balanced generalized Youden designs. 
}
\usage{
williams.BIB(d)
}
\arguments{
  \item{d}{A matrix representing a BIBD. Rows represent blocks (subjects).}
}
\details{
  For each row of the design, a Williams design is constructed using the treatments of that row.
  The rows of the resulting designs are then combined. 
  The treatments are numbered 1,\ldots,\eqn{trt}{trt}. The entry \eqn{(i,j)}{(i,j)} 
  of the design corresponds to the treatment the \eqn{i}{i}-th subject gets in the \eqn{j}{j}-th period.
}

\value{
   A matrix representing the experimental design.
}

\references{ 
Patterson, H.D. (1951): Change-over trials. Journal of the Royal Statistical 
Society B 13, 256-271. 

Wakeling, I.N. and MacFie, H.J.H. (1995): Designing consumer trials balanced for first and higher orders of
carry-over effect when only a subset of k samples from t may be tested. Food Quality and Preference 6, 299-308. 
}
\author{ Oliver Sailer }
\note{  BIBDs may be generated using \code{find.BIB}. }

\section{Warning }{ The resultig design is only balanced properly if the input design actually is a BIBD.
 This is NOT checked automatically. You have to do this by yourself, e.g. by applying \code{isGYD} to your design.
} 

\seealso{  \code{\link{get.plan}}, \code{\link{isGYD}}, \code{\link{find.BIB}}, \code{\link{williams}} }
\examples{
d <- matrix( rep(1:3,each=2), ncol=2)
# # check for balance
# isGYD(d)
williams.BIB(d)
}

\keyword{ design }% at least one, from doc/KEYWORDS
