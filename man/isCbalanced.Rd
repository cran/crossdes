\name{isCbalanced}
\alias{isCbalanced}
\title{Checking Block Designs for Carryover Balance}
\description{
   The function checks whether a block design is balanced for first order carryover effects (residual effects).
   The user specifies whether there is a preperiod. The design is checked and the left neighbour incidence matrix is given.  
}
\usage{
isCbalanced(d, preperiod = FALSE)
}
\arguments{
  \item{d}{ A matrix with entries 1,\ldots,\eqn{trt}{trt} representing the experimental design with
  rows as blocks (subjects). 
  The columns represent periods.
}
  \item{preperiod}{ Logical flag. TRUE if there is a preperiod. In this case, each subject experiences in the first period 
   the residual effect of the treatment of the last period (i.e. the last period preceeds the first period, i.e. the 
   plots in the last period are left neighbours of the plots in the first period). 
   FALSE if there are no residual effects in the first period.}
}
\details{
  The design is said to be carryover balanced (balanced for first order carryover effects), if each treatment is preceeded by all other treatments equally often
  and if no treatment is preceeded by itself. 
  If the design is balanced, this is stated.
}
\value{
  \item{1 }{Logical flag. TRUE if the design is carryover balanced. This is not displayed on the screen.}
  \item{2 }{Left neighbour incidence matrix. The \eqn{(i,j)}{(i,j)}-th element is the number of times that 
  treatment \eqn{i}{i} preceeds treatment \eqn{j}{j}.}
}

\author{ Oliver Sailer \email{sailer@statistik.uni-dortmund.de} }

\seealso{ \code{\link{isGYD}} }
\examples{
d1 <- matrix( c(1,2,3,4,1,1,1,1), 4,2)
d2 <- matrix( c(1:4,2:4,1,4,1:3,3,4,1,2),ncol=4)
d3 <- matrix( rep(1:3,each=2), ncol=2)
isCbalanced(d1)
isCbalanced(d1,TRUE)  
isCbalanced(d2)
isCbalanced(d3,TRUE)
}


\keyword{ design }% at least one, from doc/KEYWORDS
