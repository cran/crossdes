\name{isGYD}
\alias{isGYD}
\title{ Checking Row-Column Designs for Balance}
\description{
 A function to check a row-column design for balance. The rows and columns of the design
 are blocking variables. It is checked, which type of balance the design fulfills. 
 Optionally, incidence matrices are given.  
}
\usage{
isGYD(d, invis = FALSE, tables = FALSE, coded = FALSE)
}
\arguments{
  \item{d}{ A matrix representing the experimental design. The treatments must be numbered 1,\ldots,\eqn{trt}{trt}. }
  \item{invis}{ Logical flag. If TRUE, the type of design is displayed. }
  \item{tables}{ Logical flag. If TRUE, incidence matrices are given. }
  \item{coded}{ Logical flag. If TRUE, a logical vector representing the type of design is displayed.}
}
\details{
  A design is said to be a balanced block design if the following three conditions hold:
  i)   Each treatment appears equally often in the design.
  ii)  The design is binary, i.e. each treatment appears in each block either \eqn{n}{n} or \eqn{n}{n}+1 times
       where \eqn{n}{n} is an integer. 
  iii) The number of blocks where treatments \eqn{i}{i} and \eqn{j}{j} each occur \eqn{n}{n}+1 times is the same for all
       pairs of distinct treatments \eqn{(i,j)}{(i,j)}.
  Here the blocks are either rows or columns.
  
  A design that has less columns (rows) than treatments is said to be incomplete with respect to 
  rows (columns). 
  A design that is balanced with respect to both rows and columns is called a generalized Youden design (GYD).
  A GYD for which each treatment occurs equally often in each row (column) is called uniform on the rows (columns).
  If both conditions hold, it is called a generalized latin square. 
  A design where each treatment occurs exactly once in each row and column is called a latin square. 
}
\value{
  If \code{coded} is TRUE, a logical vector of length 8 is retured. The type of design is coded by this vector.
  If \code{coded} is FALSE, no value is returned.
  If \code{invis} is FALSE, the type of design is displayed on the screen.
  If \code{tables} is TRUE, additional incidence tables (occurences of treatments and pairs of treatments) are displayed.
}

\author{Oliver Sailer \email{sailer@statistik.uni-dortmund.de}}

\seealso{ \code{\link{isCbalanced}} }
\examples{
d1 <- matrix( c(1,2,3,4,1,1,1,1), 4,2)          
# d1 is not balanced
d2 <- matrix( c(1:4,2:4,1,4,1:3,3,4,1,2),ncol=4)    
# d2 is a latin square
d3 <- matrix( rep(1:3,each=2), ncol=2)          
# d3 is a balanced incomplete block design.
d1
isGYD(d1,tables=TRUE)  
d2
isGYD(d2,tables=TRUE)
d3
isGYD(d3,tables=TRUE)    
}

\keyword{design }% at least one, from doc/KEYWORDS
