"random.RT" <-
function( d ){
  trt <- max(d)
  b   <- nrow(d)
  k   <- ncol(d)
  d <- d[sample(b),]                        # Permutes rows
  a <- sample(trt)                          # Permutes treatment labels
  matrix(a[d],nc=k)}

  
