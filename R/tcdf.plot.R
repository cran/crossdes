"tcdf.plot" <-
function( empirical, n, nu, title="", xlab="", ylab="Cumulative Distribution Function",
    ltyempir=1, ltytheor=2, col1="red", col2="black", lwd=2, ... ){  
 
  x<-((1:n)-.5)/n                                # Compute quantiles at these values

  au <- .5/n                                     # Upper and lower values
  ao <- 1-.5/n
  bu <- min(empirical)
  bo <- max(empirical)

  plot( c( qt(au,nu)-1, qt(x,nu), qt(ao,nu)+1), c(0,x,1), 
    xlim=c( min(qt(au,nu),bu)-1, max(qt(ao,nu),bo)+1 ), 
    main=title, xlab=xlab, ylab=ylab,
    type="l", lty=ltytheor, lwd=lwd, col = col1, ...)  
                                                 # (theoretical) CDF

  lines( c(bu-1,sort(empirical),bo+1), c(0,x,1), type="l", lty=ltyempir, lwd=lwd, col=col2 )  
                                                 # empirical CDF
  
  legend( min(qt(au,nu),bu)-.75, .95, c("empirical","t"), 
    lty=c(ltyempir,ltytheor), lwd=lwd, col=c(col2,col1) )

}
