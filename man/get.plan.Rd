\name{get.plan}
\alias{get.plan}
\title{ Menu-Driven Construction of Carryover Balanced Experimental Designs}
\description{
  This menu based function constructs and randomizes simple experimental designs for repeated measurements with one or two block 
variables. It is assumed that each subject is assigned to each treatment at most once. A maximum number of 
subjects in the study is also requested. There are up to five possible construction methods available.
These construction methods and the characteristics of the resulting designs are described in Wakeling and MacFie (1995). }
\usage{
get.plan(trt, k = trt, maxsub = 1000)
}
\arguments{
  \item{trt}{ An integer > 1, giving the number of treatments.}
  \item{k}{ An integer in \{2,\ldots,\eqn{trt}{trt}\} giving the number of periods.}
  \item{maxsub}{ The maximum number of subjects available. }
}
\details{
  The five types of designs are: designs based on all possible treatment orders ("All combinations"),
  Williams designs ("Williams"), designs based on 
  mutually orthogonal latin squares ("MOLS"), a combination of balanced incomplete block designs (BIBDs) and Williams designs 
  ("Patterson") and
  the balanced minimal designs of Afsarinejad ("no BBD needed"). 
  Some designs are only available for special combinations of treatment number and number of periods. Other designs
  may require too many subjects. Therefore, the possible choices available for the submittet values of \eqn{trt}{trt},
  \eqn{k}{k} and \eqn{maxsub}{maxsub} 
  are determined. 
  If there is no design available, the parameters may be changed interactively.
  If more than one design type is available the user has to choose one. The minimum number of subjects required 
  for the designs is given and may be a criterion for selecting a design. All types of designs are balanced for first-order carryover effects.
  All types except the balanced minimal RMDs ("no BBD needed") are also balanced block designs. 
  The user may want to construct a design for a multiple of the minimum number of subjects required to get closer
  to the preferred number of subjects. 
  Once the design is chosen, the labels for the treatments and subjects are randomized and the design is displayed.  
  The treatments are numbered 1,\ldots,\eqn{trt}{trt}. The entry \eqn{(i,j)}{(i,j)} 
  of the design corresponds to the treatment the \eqn{i}{i}-th subject gets in the \eqn{j}{j}-th period.
}
\value{
  A matrix representing the experimental design.
}


\references{ 
  Afsarinejad, K. (1983): Balanced repeated measurements designs. Biometrika 70, 199-204.  
  
  Patterson, H.D. (1951): Change-over trials. Journal of the Royal Statistical Society B 13, 256-271.
            
  Patterson, H.D. (1952): The construction of balanced designs for experiments involving sequences of treatments. 
  Biometrika 39, 32-48.
  
  Wakeling, I.N. and MacFie, H.J.H. (1995): Designing consumer trials balanced for first and higher orders of
  carry-over effect when only a subset of k samples from t may be tested. Food Quality and Preference 6, 299-308. 
  
  Williams, E. J. (1949): Experimental designs balanced for the estimation of residual effects
  of treatments. Australian Journal of Scientific Research, Ser. A 2, 149-168. 
}



\author{Oliver Sailer \email{sailer@statistik.uni-dortmund.de}}
\note{  The "All combinations" approach requires the package \code{gtools}. }

\section{Warning }{ 
 There is a possible problem with this implementation of the "Patterson" approach:
 
 For the construction of designs that combine BIBDs with Williams designs, the function \code{find.BIB} is called
 to search for a BIBD. If the necessary conditions for the existence of a BIBD are fulfilled, the "Patterson" approach
 always returns a design. This design will however not always be a BIBD! 
 When using the patterson approach, please check the reulting design for balance using \code{isGYD} and 
 \code{isCbalanced}.
 
 It should be noted that this is a computational problem only, not a problem of the theoretical
 approach of Patterson (1951). 
 }

\seealso{ \code{\link{all.combin}}, \code{\link{balmin.RMD}}, \code{\link{des.MOLS}}, 
  \code{\link{williams}}, \code{\link{williams.BIB}} }
\examples{
\dontrun{
get.plan(10,4,60)  
  # "Patterson" or "no BBD needed"
get.plan(7,7,7000) 
  # "All combinations", "Williams" or "MOLS", "Williams" requires 
  # only 14 subjects, "All combinations" requires 5040.   
get.plan(5,5,5)    
  # Increase maxsub
}
}



\keyword{design }% at least one, from doc/KEYWORDS
