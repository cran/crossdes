"tcdf.plot" <-
function( empirical, n, nu, titel="", xaxis="", ltytheor=2, col1="red", col2="black", ... ){  
 
  x<-((1:n)-.5)/n                                # Compute quantiles at these values

  au <- .5/n                                     # Upper and lower values
  ao <- 1-.5/n
  bu <- min(empirical)
  bo <- max(empirical)

  plot( c( qt(au,nu)-1, qt(x,nu), qt(ao,nu)+1), c(0,x,1), 
    xlim=c( min(qt(au,nu),bu)-1, max(qt(ao,nu),bo)+1 ), 
    type="l", col = col1, xlab= xaxis, ylab="Cumulative distribution function", main=titel,lwd=2,lty=ltytheor, ...)  
                                                 # (theoretical) CDF

  lines( c(bu-1,sort(empirical),bo+1), c(0,x,1), type="l", col=col2,lwd=2 )  
                                                 # empirical CDF
  
  legend( min(qt(au,nu),bu)-.75, .95, c("empirical","t"),lty=c(1,ltytheor),lwd=2,col=c(col2,col1) )

}
