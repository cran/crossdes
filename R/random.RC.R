"random.RC" <-
function( d ){
  trt <- max(d)
  b   <- nrow(d)
  k   <- ncol(d)
  a   <- sample(trt)                        
  for(i in 1:b){
    d[i,] <- a[d[i,]]                       # Permutes treatment labels (the lables 1:trt change to a)
  }
  d <- d[sample(b),]                        # Permutes rows
  list(d,a)}
