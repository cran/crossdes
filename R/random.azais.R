"random.azais" <-
function( d ){
  trt <- max(d)
  b   <- nrow(d)
  k   <- ncol(d)
  for(i in 1:b){
    r <- sample(k,1) 
    if(r!=1){d[i,]<-c( d[i,r:k] , d[i,1:(r-1)] )} # Permutes order cyclically
    }
  d <- d[sample(b),]                        # Permutes rows
  a <- sample(trt)                          # Permutes treatment labels
  matrix(a[d],nc=k)}
