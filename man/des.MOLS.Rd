\name{des.MOLS}
\alias{des.MOLS}
\title{Construction of Designs Based on Mutually Orthogonal Latin Squares}
\description{
  The function constructs row-column designs based on complete sets of mutually orthogonal latin squares.
  Each subject may get each tratment at most once. The design is a generalized Youden design that is also 
  balanced for carryover effects.
}
\usage{
des.MOLS(trt, k = trt)
}
\arguments{
  \item{trt}{ A prime power less than 100. The number of treatments (products) to be tested. }
  \item{k}{ An integer \eqn{\leq \, trt}{<= trt}. Number of periods for each subject. }
}
\details{
  A complete set of mutually orthogonal latin squares is constructed using Galois Fields. 
  The rows of the designs represent the treatment
  orders for the subjects. If an incomplete design with \eqn{k}{k} columns is needed, 
  only the first \eqn{k}{k} columns of the designs are 
  considered.   
  The treatments are numbered 1,\ldots,\eqn{trt}{trt}. The entry \eqn{(i,j)}{(i,j)} 
  of the design corresponds to the treatment the \eqn{i}{i}-th subject gets in the \eqn{j}{j}-th period.
} 

\value{
  A matrix with \eqn{trt(trt-1)}{trt(trt-1)} rows and \eqn{k}{k} columns representing the experimental design.
}
\references{ 
Wakeling, I.N. and MacFie, H.J.H. (1995): Designing consumer trials balanced for first and higher orders of
 carry-over effect when only a subset of k samples from t may be tested. Food Quality and Preference 6, 299-308. 
 
Williams, E. J. (1949): Experimental designs balanced for the estimation of residual effects
of treatments. Australian Journal of Scientific Research, Ser. A 2, 149-168. 
}
\author{ Oliver Sailer \email{sailer@statistik.uni-dortmund.de}}
%\note{ Requires the package \code{conf.design} to check whether the number of treatments is a prime power. }
%

\seealso{ \code{\link{get.plan}}, \code{\link{MOLS}} }
\examples{
des.MOLS(7,7)
des.MOLS(8,5)
}

\keyword{ design }% at least one, from doc/KEYWORDS
