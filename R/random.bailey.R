"random.bailey" <-
function( d ){
  trt <- max(d)
  b   <- nrow(d)
  k   <- ncol(d)
  a   <- sample(trt)
  for(i in 1:b){
    d[i,] <- a[d[i,]]                       # Permutes treatment labels
  }
  d <- d[sample(b),]                        # Permutes rows
  d}
