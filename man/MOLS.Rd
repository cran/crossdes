\name{MOLS}
\alias{MOLS}
\title{Construction of Complete Sets of Mutually Orthogonal Latin Squares}
\description{
  The function constructs sets of mutually othogonal latin squares (MOLS) using Galois fields.
  The construction works for prime powers only. 
}
\usage{
MOLS(p, n, primpol = GF(p, n)[[2]][1, ])
}
\arguments{
  \item{p}{ A prime number less than 100. }
  \item{n}{ A positive integer.}
  \item{primpol}{ A primitive polynomial of the Galois Field GF(\eqn{p^n}{p^n}).}
}
\details{
  If \eqn{trt = p^n}{trt = p^n} is a prime power, then \eqn{trt}{trt}-1 latin squares of order \eqn{trt}{trt} 
  are constructed.
  The elements of the squares are numbered 1,\ldots,\eqn{trt}{trt}. 
  These squares are mutually orthogonal, i.e. if any two of them are superimposed, the resulting
  array will contain each ordered pair \eqn{(i,j)}{(i,j)}, \eqn{i}{i},\eqn{j}{j} in \{1,\ldots,
  \eqn{trt}{trt}\} exactly once.
  The squares are in standard order, i.e. the first row is always equal to (1,\ldots,\eqn{trt}{trt}).
  A primitive polynomial may be constructed automatically using the internal function \code{GF}.
}
\value{
  For \eqn{trt = p^n}{trt = p^n}, an array that contains \eqn{trt}{trt}-1 latin squares is returned. 
}
\references{ 
  Cherowitzo, W.: http://www-math.cudenver.edu/~wcherowi/courses/finflds.html
  
  Street, A.P. and Street, D.J. (1987): Combinatorics of experimental design. Oxford University Press, Oxford.

}
\author{ Oliver Sailer \email{sailer@statistik.uni-dortmund.de}}


\seealso{  \code{\link{des.MOLS}} }
\examples{
MOLS(7,1) # 6 mutually orthogonal latin squares of order 7
MOLS(2,3) # 7 mutually orthogonal latin squares of order 8
}

\keyword{ design }% at least one, from doc/KEYWORDS
