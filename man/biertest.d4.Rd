\name{biertest.d4}
\alias{biertest.d4}
\docType{data}
\title{Experimental Design for the Beer Testing Data in Kunert (1998)}
\description{
  The design is a balanced incomplete block design with rows 
  as blocks. If one assumes that there is a preperiod, i.e. that each assessor is given the treatment of the fifth period
  before the experiment starts, then the design is carryover balanced. 
}
\usage{data(biertest.d4)}
\format{
  A matrix with 12 rows corresponding to the assessors, 5 columns corresponding to 
  the five periods. 
}
\details{
The five treatments (products) to be tested are numbered 1,\ldots,5.
}

\source{
  Kunert, J. (1998): Sensory experiments as crossover studies. Food Quality and Preference 9, 243-253
  (design d4). 
}

\examples{
data(biertest.d4)
}
\keyword{datasets}
