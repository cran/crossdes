"random.azais" <-
function( d ){
  trt <- max(d)
  b   <- nrow(d)
  k   <- ncol(d)
  a   <- sample(trt)
  for(i in 1:b){
    d[i,] <- a[d[i,]]                       # Permutes treatment labels
    r <- sample(k,1) 
    if(r!=1){d[i,]<-c( d[i,r:k] , d[i,1:(r-1)] )} # Permutes order cyclically
    }
  d <- d[sample(b),]                        # Permutes rows
  d}
